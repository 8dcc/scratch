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
 * Credits:
 * - https://beej.us/guide/bgnet/html/
 */

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include <arpa/inet.h> /* inet_ntop */
#include <netdb.h>     /* struct addrinfo */

static struct addrinfo* get_info(const char* target) {
    /*
     * We initialize an `addrinfo' structure with the hints for `getaddrinfo'
     * (its 3rd parameter).
     *
     * First, the address family: IPv4 (AF_INET).
     * Second, the socket type: TCP (SOCK_STREAM).
     */
    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    /*
     * Obtain the target information as a linked list in `target_info', using
     * the `hints' we just defined.
     */
    struct addrinfo* target_info;
    const int status = getaddrinfo(target, NULL, &hints, &target_info);
    if (status != 0) {
        fprintf(stderr, "Error getting information: %s\n",
                gai_strerror(status));
        return NULL;
    }

    /*
     * The returned pointer should be freed by the caller with
     * `free_info' (i.e. `freeaddrinfo').
     */
    return target_info;
}

static inline void free_info(struct addrinfo* info) {
    freeaddrinfo(info);
}

/*----------------------------------------------------------------------------*/

static const char* get_ip_from_addrinfo(const struct addrinfo* addr_info) {
    static char ip_str[INET6_ADDRSTRLEN];

    /*
     * To obtain the IP address, we use the `in_addr' struct (it used to
     * be a union in the past), which is inside the `sockaddr' structure (it
     * can be safely cast to a `sockaddr_in' pointer, since it's more
     * convenient) which is inside the `addrinfo' structure (each element of
     * the list we are currently iterating).
     *
     * The `in_addr' structure contains the actual 32-bit integer names
     * `s_addr', but we won't use it directly:
     *
     *     addrinfo -> sockaddr(_in) -> in_addr [-> s_addr]
     *
     * Note that, for IPv6 addresses, `in6_addr' is used instead of `in_addr',
     * and `sockaddr_in6' is used instead of `sockaddr_in'.
     */
    void* ip;
    switch (addr_info->ai_family) {
        case AF_INET: {
            struct sockaddr_in* addr_data =
              (struct sockaddr_in*)addr_info->ai_addr;
            struct in_addr* ipv4 = &addr_data->sin_addr;
            ip                   = ipv4;
        } break;

        case AF_INET6: {
            struct sockaddr_in6* addr_data =
              (struct sockaddr_in6*)addr_info->ai_addr;
            struct in6_addr* ipv6 = &addr_data->sin6_addr;
            ip                    = ipv6;
        } break;

        default:
            return NULL;
    }

    /* Convert the IP to a string */
    return inet_ntop(addr_info->ai_family, ip, ip_str, sizeof(ip_str));
}

static void print_info(const struct addrinfo* addr_info) {
    /* Traverse the linked list of `addrinfo' structures */
    for (; addr_info != NULL; addr_info = addr_info->ai_next) {
        const char* ip_str = get_ip_from_addrinfo(addr_info);
        if (ip_str == NULL)
            continue;

        puts(ip_str);
    }
}

/*----------------------------------------------------------------------------*/

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s \"DOMAIN-OR-IP\"\n", argv[0]);
        return 1;
    }

    struct addrinfo* addr_info = get_info(argv[1]);
    print_info(addr_info);
    free_info(addr_info);

    return 0;
}
