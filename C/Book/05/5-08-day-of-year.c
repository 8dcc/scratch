
#include <stdio.h>

int day_of_year(int year, int month, int day);
void month_day(int year, int yearday, int* pmonth, int* pday);

int main() {
    int d1 = day_of_year(2022, 11, 16);
    printf("Day: %d\n", d1);

    int month = 0;
    int day   = 0;
    month_day(2022, 223, &month, &day);
    printf("Month: %d, Day: %d\n", month, day);

    return 0;
}

static char daytab[2][13] = { 
    { 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
    { 0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
};

/* day_of_year: set day of year from month & day */
int day_of_year(int year, int month, int day) {
    if (year < 1 || month < 1 || month > 12 || day < 1 || day > 31)
        return 0;

    // Will access seccond sub-array of main array (length in leap years) if true (1)
    int leap = (year % 4 == 0 && year % 100 != 0) || year % 400 == 0;

    for (int i = 1; i < month; i++)
        day += daytab[leap][i];

    return day;
}

/* month_day: set month, day from day of year */
void month_day(int year, int yearday, int* pmonth, int* pday) {
    if (year < 1 || yearday < 1 || yearday > 366)
        return;

    int leap = (year % 4 == 0 && year % 100 != 0) || year % 400 == 0;

    int i;
    for (i = 1; yearday > daytab[leap][i]; i++)
        yearday -= daytab[leap][i];
    *pmonth = i;
    *pday   = yearday;
}
