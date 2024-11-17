/*
 * Copyright 2024 8dcc
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program. If not, see <https://www.gnu.org/licenses/>.
 *
 * ----------------------------------------------------------------------------
 *
 * NOTE: For a commented version, see "main1.c".
 */

#include <stdio.h>

#include "util.h"
#include "liblog.h"

#define TARGET_MODULE_REGEX ".*BlackOps3.exe$"

/*
 * This offset is used to toggle chams.
 *
 * TODO: What does this point to, exactly? What are these bytes?
 *
 * - 01B0: Enabled
 * - C032: Disabled.
 */
#define TARGET_OFFSET 0x8FE2B6

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

    void* base_address = getModuleBaseAddress(pid, TARGET_MODULE_REGEX);
    if (!base_address) {
        log_ftl("Couldn't find base address of module matching regex.");
        return 1;
    }
    log_dbg("Module base address: %p", base_address);

    void* target_address = GET_OFFSET(base_address, TARGET_OFFSET);
    log_dbg("Final address: %p", target_address);

    uint8_t bytes[2];
    for (;;) {
        readProcessMemory(pid, target_address, bytes, sizeof(bytes));
        printf("Current bytes: %02X %02X\n", bytes[0], bytes[1]);

        printf("Press enter to toggle them...");
        while (getchar() != '\n')
            ;

        if (bytes[0] == 0x32 && bytes[1] == 0xC0) {
            /* TODO: ??? -> ??? */
            bytes[0] = 0xB0;
            bytes[1] = 0x01;
        } else if (bytes[0] == 0xB0 && bytes[1] == 0x01) {
            /* TODO: ??? -> ??? */
            bytes[0] = 0x32;
            bytes[1] = 0xC0;
        } else {
            log_ftl("Unknown bytes.");
            return 1;
        }

        writeProcessMemory(pid, target_address, bytes, sizeof(bytes));
    }

    return 0;
}
