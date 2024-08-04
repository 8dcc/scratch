
#ifndef UTIL_H_
#define UTIL_H_ 1

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h> /* printf() */
#include <regex.h> /* regex_t */

#include "liblog.h" /* log_err() */

#define ERR(...) log_err(__VA_ARGS__)

#define GET_OFFSET(BASE, OFFSET) ((void*)((uintptr_t)BASE + OFFSET))

/*----------------------------------------------------------------------------*/

typedef struct ModuleBounds {
    void* start;
    void* end;
    struct ModuleBounds* next;
} ModuleBounds;

/*----------------------------------------------------------------------------*/

void readProcessMemory(pid_t pid, void* addr, void* out, size_t sz);
void writeProcessMemory(pid_t pid, void* addr, void* data, size_t sz);

ModuleBounds* getModuleBounds(int pid, const char* regex);
void freeModuleBounds(ModuleBounds* bounds);
void* getModuleBaseAddress(int pid, const char* regex);

#endif /* UTIL_H_ */
