// GPTMAX
// parse_number.c
// Compile: gcc -std=c11 -O2 parse_number.c -lm -o parse_number
#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <math.h>

bool parse_number(const char *s, size_t len, double *out) {
    if (!s || len == 0 || !out) return false;
    size_t i = 0;

    // optional sign
    int sign = 1;
    if (i < len && (s[i] == '+' || s[i] == '-')) {
        if (s[i] == '-') sign = -1;
        i++;
        if (i == len) return false; // only sign -> invalid
    }

    // Check for hex: 0x or 0X immediately after optional sign
    if (i + 1 < len && s[i] == '0' && (s[i+1] == 'x' || s[i+1] == 'X')) {
        i += 2;
        if (i == len) return false; // "0x" only -> invalid
        double val = 0.0;
        bool any = false;
        for (; i < len; ++i) {
            char c = s[i];
            int digit = -1;
            if (c >= '0' && c <= '9') digit = c - '0';
            else if (c >= 'a' && c <= 'f') digit = 10 + (c - 'a');
            else if (c >= 'A' && c <= 'F') digit = 10 + (c - 'A');
            else break;
            any = true;
            val = val * 16.0 + (double)digit;
        }
        if (!any) return false;
        // There must be no extra characters
        if (i != len) return false;
        *out = sign * val;
        return true;
    }

    // Decimal / floating point / scientific
    // integer part
    double int_part = 0.0;
    size_t int_digits = 0;
    while (i < len && isdigit((unsigned char)s[i])) {
        int_part = int_part * 10.0 + (double)(s[i] - '0');
        i++; int_digits++;
    }

    // fractional part
    double frac = 0.0;
    size_t frac_digits = 0;
    if (i < len && s[i] == '.') {
        i++;
        while (i < len && isdigit((unsigned char)s[i])) {
            frac = frac * 10.0 + (double)(s[i] - '0');
            i++; frac_digits++;
        }
    }

    // if there were no integer digits and no fractional digits, it's invalid
    if (int_digits == 0 && frac_digits == 0) return false;

    double value = int_part;
    if (frac_digits > 0) {
        value += frac / pow(10.0, (double)frac_digits);
    }

    // exponent part
    if (i < len && (s[i] == 'e' || s[i] == 'E')) {
        i++;
        if (i == len) return false; // trailing 'e' is invalid
        int exp_sign = 1;
        if (s[i] == '+' || s[i] == '-') {
            if (s[i] == '-') exp_sign = -1;
            i++;
            if (i == len) return false; // only sign after e
        }
        // parse exponent digits
        int exp_val = 0;
        size_t exp_digits = 0;
        while (i < len && isdigit((unsigned char)s[i])) {
            // guard exp overflow by capping to reasonable value
            if (exp_val < 1000000000) { // large cap to avoid UB
                exp_val = exp_val * 10 + (s[i] - '0');
            }
            i++; exp_digits++;
        }
        if (exp_digits == 0) return false;
        double pow10 = pow(10.0, (double)(exp_sign * exp_val));
        value *= pow10;
    }

    // no extra chars allowed
    if (i != len) return false;

    *out = sign * value;
    return true;
}

/* Quick test harness */
// #define TEST_PARSE_NUMBER
#ifdef TEST_PARSE_NUMBER
int main(void) {
    const char *tests[] = {
        "2", "2.5", "020203", "0xfF0d", "1e9", "-1.2e-3", ".5", "2.", "+3.14", "0X10", "0x", "abc", "1e"
    };
    for (size_t t = 0; t < sizeof(tests)/sizeof(tests[0]); ++t) {
        const char *str = tests[t];
        double v;
        bool ok = parse_number(str, strlen(str), &v);
        printf("'%s' -> %s", str, ok ? "OK" : "ERR");
        if (ok) printf(" : %.17g", v);
        printf("\n");
    }
    return 0;
}
#endif

