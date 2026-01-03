#include "parser.h"
#include <stdio.h>

void err_sym_exists(Name name) {
    err("symbol \"%.*s\" already exists.",
        (int)name.length,name.name);
}

// assumes an ideal type struct (to free)
#define TAG_CMPT "[-cmpt-] "
void _cmptime_log_caller(const char *fmt, ...) {
    char buf[1024];
    va_list args;
    va_start(args, fmt);
    int len = format_log(
        buf, sizeof(buf),
        TAG_CMPT,
        COLOR_DEBUG,
        fmt,
        args
    );
    va_end(args);

    if (len > 0)
        write_log(2, buf, (size_t)len);
}

