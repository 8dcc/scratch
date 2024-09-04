/*
 * Credits:
 *   - https://stackoverflow.com/a/1598774/11715554
 *   - https://stackoverflow.com/a/1594039/11715554
 *   - https://stackoverflow.com/a/20262010/11715554
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <ifaddrs.h>    /* getifaddrs(), etc. */
#include <sys/socket.h> /* struct sockaddr */
#include <net/if.h>     /* IFF_LOOPBACK */
#include <netdb.h>      /* getnameinfo(), etc. */

int main(void) {
    struct ifaddrs* ifaddr;
    if (getifaddrs(&ifaddr) == -1) {
        perror("getifaddrs");
        exit(1);
    }

    for (struct ifaddrs* ifa = ifaddr; ifa != NULL; ifa = ifa->ifa_next) {
        /* Ignore non-INET address families and loopback interfaces */
        if (ifa->ifa_addr->sa_family != AF_INET ||
            (ifa->ifa_flags & IFF_LOOPBACK) != 0)
            continue;

        char host[NI_MAXHOST];
        int code = getnameinfo(ifa->ifa_addr, sizeof(struct sockaddr_in), host,
                               NI_MAXHOST, NULL, 0, NI_NUMERICHOST);
        if (code != 0) {
            fprintf(stderr, "getnameinfo() failed: %s\n", gai_strerror(code));
            exit(1);
        }

        printf("%-10s %s\n", ifa->ifa_name, host);
    }

    freeifaddrs(ifaddr);

    return 0;
}
