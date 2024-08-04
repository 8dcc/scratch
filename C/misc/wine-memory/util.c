/*
 * NOTE: This source is basically the same one from my CS:Source external cheat.
 * See: https://github.com/8dcc/cs-source-external
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* Needed for process_vm_readv() in uio.h */
#endif

#include <stdio.h>   /* fopen(), FILE* */
#include <string.h>  /* strstr() */
#include <stdlib.h>  /* atoi() */
#include <regex.h>   /* regcomp(), regexec(), etc. */
#include <dirent.h>  /* readdir() */
#include <sys/uio.h> /* process_vm_readv() */
#include <errno.h>   /* errno */

#include "util.h"

/*----------------------------------------------------------------------------*/

void readProcessMemory(pid_t pid, void* addr, void* out, size_t sz) {
    /* The function expects an array, even though our array has one element */
    struct iovec local[1];
    struct iovec remote[1];

    local[0].iov_base  = out;
    local[0].iov_len   = sz;
    remote[0].iov_base = addr;
    remote[0].iov_len  = sz;

    if (process_vm_readv(pid, local, 1, remote, 1, 0) == -1) {
        ERR("Error reading memory of process with ID %d. Error: %s (%d)", pid,
            strerror(errno), errno);
        exit(1);
    }
}

void writeProcessMemory(pid_t pid, void* addr, void* data, size_t sz) {
    /* The function expects an array, even though our array has one element */
    struct iovec local[1];
    struct iovec remote[1];

    local[0].iov_base  = data;
    local[0].iov_len   = sz;
    remote[0].iov_base = addr;
    remote[0].iov_len  = sz;

    if (process_vm_writev(pid, local, 1, remote, 1, 0) == -1) {
        ERR("Error writing memory of process with ID %d. Error: %s (%d)", pid,
            strerror(errno), errno);
        exit(1);
    }
}

/*----------------------------------------------------------------------------*/

/* Returns true if string `str' mathes regex pattern `pat'. Pattern uses BRE
 * syntax: https://www.gnu.org/software/sed/manual/html_node/BRE-syntax.html */
bool myRegex(regex_t expr, const char* str) {
    int code = regexec(&expr, str, 0, NULL, 0);
    if (code > REG_NOMATCH) {
        char err[100];
        regerror(code, &expr, err, sizeof(err));
        ERR("regexec() returned an error: %s\n", err);
        return false;
    }

    /* REG_NOERROR: Success
     * REG_NOMATCH: Pattern did not match */
    return code == REG_NOERROR;
}

/*----------------------------------------------------------------------------*/
/*
 * Functions for getting the bounds of a module loaded by PID.
 *
 * Credits to my own signature scanning library:
 *   https://github.com/8dcc/libsigscan
 *   https://8dcc.github.io/programming/signature-scanning.html
 */

ModuleBounds* getModuleBounds(int pid, const char* regex) {
    static regex_t compiled_regex;

    /* Compile regex pattern once here */
    if (regex != NULL && regcomp(&compiled_regex, regex, REG_EXTENDED) != 0) {
        ERR("regcomp() returned an error code for pattern \"%s\"\n", regex);
        return NULL;
    }

    /* Get the full path to /proc/PID/maps from the specified PID */
    static char maps_path[50];
    sprintf(maps_path, "/proc/%d/maps", pid);

    /* Open the maps file */
    FILE* fd = fopen(maps_path, "r");
    if (!fd) {
        ERR("Couldn't open /proc/%d/maps", pid);
        return NULL;
    }

    /* For the first module. Start `ret' as NULL in case no module is valid. */
    ModuleBounds* ret = NULL;
    ModuleBounds* cur = ret;

    /* Buffers used in the loop by fgets() and sscanf() */
    static char line_buf[300];
    static char rwxp[5];
    static char pathname[200];

    while (fgets(line_buf, sizeof(line_buf), fd)) {
        pathname[0] = '\0';

        /* Scan the current line using sscanf(). We need to change address sizes
         * depending on the arch. */
        long unsigned start_num = 0, end_num = 0, offset = 0;
        int fmt_match_num =
          sscanf(line_buf, "%lx-%lx %4s %lx %*x:%*x %*d %200[^\n]\n",
                 &start_num, &end_num, rwxp, &offset, pathname);

        if (fmt_match_num < 4) {
            ERR("sscanf() didn't match the minimum fields (4) for line:\n%s",
                line_buf);
        }

        void* start_addr = (void*)start_num;
        void* end_addr   = (void*)end_num;

        /* Parse "rwxp". For now we only care about read permissions. */
        bool is_readable = rwxp[0] == 'r';

        bool name_matches = true;
        if (regex == NULL) {
            /* We don't want to filter the module name, just make sure it
             * doesn't start with '[' and skip to the end of the line. */
            if (pathname[0] == '[')
                name_matches = false;
        } else {
            /* Compare module name against provided regex. Note that the output
             * of maps has absolute paths. */
            if (!myRegex(compiled_regex, pathname))
                name_matches = false;
        }

        /* We can read it, and it's the module we are looking for. */
        if (is_readable && name_matches) {
            if (cur == NULL) {
                /* Allocate the first bounds struct */
                cur = (ModuleBounds*)malloc(sizeof(ModuleBounds));

                /* This one will be returned */
                ret = cur;

                /* Save the addresses from this line of maps */
                cur->start = start_addr;
                cur->end   = end_addr;
            } else if (cur->end == start_addr && cur->end < end_addr) {
                /* If the end address of the last struct is the start of this
                 * one, just merge them. */
                cur->end = end_addr;
            } else {
                /* There was a gap between the end of the last block and the
                 * start of this one, allocate new struct. */
                cur->next = (ModuleBounds*)malloc(sizeof(ModuleBounds));

                /* Set as current */
                cur = cur->next;

                /* Save the addresses from this line of maps */
                cur->start = start_addr;
                cur->end   = end_addr;
            }

            /* Indicate the end of the linked list */
            cur->next = NULL;
        }
    }

    /* If we compiled a regex expression, free it before returning */
    if (regex != NULL)
        regfree(&compiled_regex);

    fclose(fd);
    return ret;
}

/* Free a linked list of ModuleBounds structures */
void freeModuleBounds(ModuleBounds* bounds) {
    ModuleBounds* cur = bounds;
    while (cur != NULL) {
        ModuleBounds* next = cur->next;
        free(cur);
        cur = next;
    }
}

void* getModuleBaseAddress(int pid, const char* regex) {
    void* result = NULL;

    ModuleBounds* bounds = getModuleBounds(pid, regex);
    if (bounds != NULL) {
        result = bounds->start;
        freeModuleBounds(bounds);
    }

    return result;
}
