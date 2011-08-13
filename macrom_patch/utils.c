#include "utils.h"

/*
 *  Convert time_t value to MacOS time (seconds since 1.1.1904)
 */


uint32_t convert_to_mac_time(time_t t)
{
    /* This code is taken from glibc 2.2 */

    /* Convert to number of seconds elapsed since 1-Jan-1904 */

    struct tm *local = localtime(&t);
    const int TM_EPOCH_YEAR = 1900;
    const int MAC_EPOCH_YEAR = 1904;

    int a4 = ((local->tm_year + TM_EPOCH_YEAR) >> 2) - !(local->tm_year & 3);
    int b4 = (MAC_EPOCH_YEAR >> 2) - !(MAC_EPOCH_YEAR & 3);
    int a100 = a4 / 25 - (a4 % 25 < 0);
    int b100 = b4 / 25 - (b4 % 25 < 0);
    int a400 = a100 >> 2;
    int b400 = b100 >> 2;
    int intervening_leap_days = (a4 - b4) - (a100 - b100) + (a400 - b400);
    uint32_t days = local->tm_yday +
                    365 * (local->tm_year - 4) +
                    intervening_leap_days;

    return local->tm_sec + 60 * (local->tm_min +
           60 * (local->tm_hour + 24 * days));
}
