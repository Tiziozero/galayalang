#ifndef PARSE_NUMBER_H
#define PARSE_NUMBER_H
#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

typedef enum {
    NumKindInt,
    NumKindFloat,
} NumKind;

typedef struct {
    NumKind kind;
    union {
        uint64_t i; // for int
        double   f; // for float
    };
    int is_negative;
} ParsedNumber;

// strip underscores into buf, return new length
static size_t strip_underscores(const char* s, size_t len, char* buf, size_t bufsz) {
    size_t out = 0;
    for (size_t i = 0; i < len && out < bufsz - 1; i++)
        if (s[i] != '_') buf[out++] = s[i];
    buf[out] = '\0';
    return out;
}

static inline bool parse_number(const char* s, size_t len, ParsedNumber* out) {
    if (!s || len == 0 || !out) return false;

    char buf[256];
    len = strip_underscores(s, len, buf, sizeof(buf));
    s = buf;

    size_t i = 0;

    int sign = 1;
    if (i < len && (s[i] == '+' || s[i] == '-')) {
        if (s[i] == '-') sign = -1;
        i++;
        if (i == len) return false;
    }

    // binary: 0b / 0B
    if (i + 1 < len && s[i] == '0' && (s[i+1] == 'b' || s[i+1] == 'B')) {
        i += 2;
        if (i == len) return false;
        uint64_t val = 0;
        bool any = false;
        for (; i < len; i++) {
            if (s[i] == '0' || s[i] == '1') {
                val = (val << 1) | (s[i] - '0');
                any = true;
            } else return false; // unexpected char
        }
        if (!any) return false;
        out->kind = NumKindInt;
        out->i = val;
        out->is_negative = (sign == -1);
        return true;
    }

    // octal: 0o / 0O
    if (i + 1 < len && s[i] == '0' && (s[i+1] == 'o' || s[i+1] == 'O')) {
        i += 2;
        if (i == len) return false;
        uint64_t val = 0;
        bool any = false;
        for (; i < len; i++) {
            if (s[i] >= '0' && s[i] <= '7') {
                val = (val << 3) | (s[i] - '0');
                any = true;
            } else return false;
        }
        if (!any) return false;
        out->kind = NumKindInt;
        out->i = val;
        out->is_negative = (sign == -1);
        return true;
    }

    // hex: 0x / 0X
    if (i + 1 < len && s[i] == '0' && (s[i+1] == 'x' || s[i+1] == 'X')) {
        i += 2;
        if (i == len) return false;
        uint64_t val = 0;
        bool any = false;
        for (; i < len; i++) {
            char c = s[i];
            int digit;
            if      (c >= '0' && c <= '9') digit = c - '0';
            else if (c >= 'a' && c <= 'f') digit = 10 + (c - 'a');
            else if (c >= 'A' && c <= 'F') digit = 10 + (c - 'A');
            else return false;
            val = (val << 4) | digit;
            any = true;
        }
        if (!any) return false;
        out->kind = NumKindInt;
        out->i = val;
        out->is_negative = (sign == -1);
        return true;
    }

    // decimal or float
    uint64_t int_part = 0;
    size_t int_digits = 0;
    while (i < len && s[i] >= '0' && s[i] <= '9') {
        int_part = int_part * 10 + (s[i] - '0');
        i++; int_digits++;
    }

    // if no dot and no exponent -> integer
    if (i == len) {
        if (int_digits == 0) return false;
        out->kind = NumKindInt;
        out->i = int_part;
        out->is_negative = (sign == -1);
        return true;
    }

    // float
    double value = (double)int_part;
    size_t frac_digits = 0;

    if (s[i] == '.') {
        i++;
        double frac = 0.0;
        while (i < len && s[i] >= '0' && s[i] <= '9') {
            frac = frac * 10.0 + (s[i] - '0');
            i++; frac_digits++;
        }
        if (frac_digits > 0)
            value += frac / pow(10.0, (double)frac_digits);
    }

    if (int_digits == 0 && frac_digits == 0) return false;

    if (i < len && (s[i] == 'e' || s[i] == 'E')) {
        i++;
        if (i == len) return false;
        int exp_sign = 1;
        if (s[i] == '+' || s[i] == '-') {
            if (s[i] == '-') exp_sign = -1;
            i++;
            if (i == len) return false;
        }
        int exp_val = 0;
        size_t exp_digits = 0;
        while (i < len && s[i] >= '0' && s[i] <= '9') {
            if (exp_val < 100000) exp_val = exp_val * 10 + (s[i] - '0');
            i++; exp_digits++;
        }
        if (exp_digits == 0) return false;
        value *= pow(10.0, (double)(exp_sign * exp_val));
    }

    if (i != len) return false;

    out->kind = NumKindFloat;
    out->f = sign * value;
    out->is_negative = (sign == -1);
    return true;
}

#ifdef TEST_PARSE_NUMBER
#include <stdio.h>
int main(void) {
    const char* tests[] = {
        "2", "2.5", "0xFF", "0b1010_1010", "0o755", "1e9", "-1.2e-3",
        "1_000_000", "0xDEAD_BEEF", "0b0", "0x", "0b2", "1e", ""
    };
    for (size_t t = 0; t < sizeof(tests)/sizeof(tests[0]); t++) {
        const char* str = tests[t];
        ParsedNumber n;
        bool ok = parse_number(str, strlen(str), &n);
        printf("'%s' -> %s", str, ok ? "OK" : "ERR");
        if (ok) {
            if (n.kind == NumKindInt)
                printf(" int: %s%llu", n.is_negative ? "-" : "", (unsigned long long)n.i);
            else
                printf(" float: %.17g", n.f);
        }
        printf("\n");
    }
    return 0;
}
#endif

#endif // PARSE_NUMBER_H
