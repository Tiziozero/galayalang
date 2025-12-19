#ifndef LOGGER_H
#define LOGGER_H

#include <unistd.h>
#include <errno.h>
#include <stddef.h>
#define COLOR_RESET "\x1b[0m"
#define COLOR_INFO  "\x1b[32m"  // green
#define COLOR_ERROR "\x1b[31m"  // red
#define COLOR_WARN  "\x1b[33m"  // yellow
#define COLOR_DEBUG "\x1b[36m"  // cyan

static inline ssize_t write_log(int fd, const char *buf, size_t len)
{
    size_t total = 0;

    while (total < len) {
        ssize_t n = write(fd, buf + total, len - total);
        if (n < 0) {
            if (errno == EINTR)
                continue;
            return -1;
        }
        total += n;
    }

    return (ssize_t)total;
}

#include <stdarg.h>
#include <stdio.h>

static int format_log(
    char *buf,
    size_t bufsize,
    const char *prefix,
    const char *color,
    const char *fmt,
    va_list args
)
{
    int n = snprintf(buf, bufsize, "%s%s%s", color, prefix, COLOR_RESET);
    if (n < 0 || (size_t)n >= bufsize)
        return -1;

    int m = vsnprintf(
        buf + n,
        bufsize - n,
        fmt,
        args
    );
    if (m < 0 || (size_t)(n + m) >= bufsize)
        return -1;

    int r = snprintf(
        buf + n + m,
        bufsize - n - m,
        "\x1b[0m\n"
    );
    if (r < 0)
        return -1;

    return n + m + r;
}

#include <stdarg.h>

#define TAG_INFO   "[INFO] "

static inline void info(const char *fmt, ...)
{
    char buf[1024];

    va_list args;
    va_start(args, fmt);
    int len = format_log(
        buf, sizeof(buf),
        TAG_INFO,
        COLOR_INFO,
        fmt,
        args
    );
    va_end(args);

    if (len > 0)
        write_log(2, buf, (size_t)len);
}

#define TAG_ERR   "[ERROR] "

static inline void err(const char *fmt, ...)
{
    char buf[1024];

    va_list args;
    va_start(args, fmt);
    int len = format_log(
        buf, sizeof(buf),
        TAG_ERR, 
        COLOR_ERROR,
        fmt,
        args
    );
    va_end(args);

    if (len > 0)
        write_log(2, buf, (size_t)len);
}


#endif // LOGGER_H
