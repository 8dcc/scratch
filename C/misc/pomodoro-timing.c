/*
 * The pomodoro cycle is usually the following:
 *   - Work for 25 minutes.
 *   - Rest for 5 minutes.
 *   - Work for 25 minutes.
 *   - Rest for 5 minutes.
 *   - Work for 25 minutes.
 *   - Rest for 5 minutes.
 *   - Work for 25 minutes.
 *   - Rest for 30 minutes.
 * The timings might vary, but that's the general idea.
 *
 * This function calculates if a minute is in a pomodoro (work interval) or not
 * (resting).
 *
 * See also: https://en.wikipedia.org/wiki/Pomodoro_Technique
 */

#include <stdbool.h>
#include <stdio.h>

static bool in_pomodoro(int minutes) {
    /* The position in the current 145 minute cycle */
    int minutes_in_clycle = minutes % ((25 + 5) * 3 + 25 + 30);

    /* Big rests start at minute 115 and end at minute 140 */
    bool in_long_rest = (minutes_in_clycle % 140 >= 115);

    /* Short rests start at minute 25 and end at minute 30 */
    bool in_short_rest = (minutes_in_clycle % 30 >= 25);

    return !in_long_rest && !in_short_rest;
}

int main(void) {
    int i;

    /* Simulate two cycles */
    printf("Starting cycle one:\n");
    for (i = 0; i <= 145; i++)
        printf("Minute: %3d -> %s\n", i,
               in_pomodoro(i) ? "Pomodoro." : "Rest.");

    printf("Starting cycle two:\n");
    for (i = 146; i <= 290; i++)
        printf("Minute: %3d -> %s\n", i,
               in_pomodoro(i) ? "Pomodoro." : "Rest.");

    return 0;
}
