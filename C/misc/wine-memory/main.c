
#include <stdio.h>

#include "util.h"
#include "liblog.h"

/*
 * NOTE: The target module is obtained by looking at the /proc/PID/maps file of
 * the process. See the list-cod-bo3-modules.sh script.
 * In this case, the target module we want to read/write is the executable
 * itself, not a DLL.
 */
#define TARGET_MODULE_REGEX ".*BlackOps3.exe$"

/*
 * In this case, this is the offset inside the module of a JMP instruction that
 * we want to patch. Specifically the one that's used to check whether or not
 * the H.A.T.R. score-streak is enabled in the mini-map.
 */
#define TARGET_OFFSET 0x59240C

/*----------------------------------------------------------------------------*/

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr,
                "Usage: %s PID\n"
                "To get the PID, use list-cod-bo3-modules.sh\n",
                argv[0]);
        return 1;
    }

    int pid = 0;
    if (sscanf(argv[1], "%d", &pid) != 1) {
        log_ftl("Invalid PID format.");
        return 1;
    }

    log_dbg("Target PID: %d", pid);
    log_dbg("Target module regex: %s", TARGET_MODULE_REGEX);

    /*
     * Get the base address of the module matching the regex by parsing
     * /proc/PID/maps.
     */
    void* base_address = getModuleBaseAddress(pid, TARGET_MODULE_REGEX);
    if (!base_address) {
        log_ftl("Couldn't find base address of module matching regex.");
        return 1;
    }
    log_dbg("Module base address: %p", base_address);

    /* Add target offset to the base address of the module */
    void* target_address = GET_OFFSET(base_address, TARGET_OFFSET);
    log_dbg("Final address: %p", target_address);

    uint8_t bytes[2];
    for (;;) {
        readProcessMemory(pid, target_address, bytes, sizeof(bytes));
        printf("Current instruction bytes: %02X %02X\n", bytes[0], bytes[1]);

        printf("Press enter to toggle them...");
        while (getchar() != '\n')
            ;

        if (bytes[0] == 0x74 && bytes[1] == 0x3C) {
            /* je 0x3E -> nop, nop */
            bytes[0] = 0x90;
            bytes[1] = 0x90;
        } else if (bytes[0] == 0x90 && bytes[1] == 0x90) {
            /* nop, nop -> je 0x3E */
            bytes[0] = 0x74;
            bytes[1] = 0x3C;
        } else {
            log_ftl("Unknown instruction bytes.");
            return 1;
        }

        /*
         * Overwrite them with the new bytes, effectively enabling or disabling
         * the JMP instruction.
         */
        writeProcessMemory(pid, target_address, bytes, sizeof(bytes));
    }

    return 0;
}
