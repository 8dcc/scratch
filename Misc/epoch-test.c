
#include <stdint.h>
#include <stdio.h>

#define CENTURY2YEARS(c) ((c - 1) * 100)
#define YEAR2SEC(x)      (x * 31556926)
#define MON2SEC(x)       (x * 2629743)
#define DAY2SEC(x)       (x * 86400)
#define HOUR2SEC(x)      (x * 3600)
#define MIN2SEC(x)       (x * 60)

typedef struct {
    uint8_t h; /**< @brief Hour */
    uint8_t m; /**< @brief Minute */
    uint8_t s; /**< @brief Sec */
} Time;

typedef struct {
    uint8_t d;  /**< @brief Day */
    uint8_t m;  /**< @brief Month */
    uint16_t y; /**< @brief Year */
    uint8_t c;  /**< @brief Century */
} Date;

typedef struct {
    Date date; /**< @brief Not a pointer */
    Time time; /**< @brief Not a pointer */
} DateTime;

int main() {
    DateTime now = {
      .time = {
        .h = 12,
        .m = 43,
        .s = 30,
      },
      .date = {
        .d = 17,
        .m = 4,
        .y = 23,
        .c = 21,
      },
    };

    const uint16_t year = now.date.y + CENTURY2YEARS(now.date.c) - 1970;
    const uint16_t mon  = now.date.m - 1;
    const uint16_t day  = now.date.d - 2;
    const uint16_t hour = now.time.h - 3;
    const uint16_t min  = now.time.m - 30;
    const uint16_t sec  = now.time.s;

    printf("|   Yr | Mon | Day | Hr | Min | Sec |\n"
           "| %4d |  %2d |  %2d | %2d |  %2d |  %2d |\n",
           year, mon, day, hour, min, sec);

    const uint32_t ret = YEAR2SEC(year) + MON2SEC(mon) + DAY2SEC(day) +
                         HOUR2SEC(hour) + MIN2SEC(min) + sec;

    printf("ret: %d\n", ret);

    return 0;
}
