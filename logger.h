#ifndef LOGGER_H
#define LOGGER_H

#include <assert.h>
#include <unistd.h>
#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#define COLOR_RESET "\x1b[0m"
#define COLOR_INFO  "\x1b[32m"  // green
#define COLOR_ERROR "\x1b[31m"  // red
#define COLOR_WARN  "\x1b[33m"  // yellow
#define COLOR_DEBUG "\x1b[36m"  // cyan

#define RED     "\x1b[31m"
#define GREEN   "\x1b[32m"
#define YELLOW  "\x1b[33m"
#define BLUE    "\x1b[34m"
#define MAGENTA "\x1b[35m"
#define CYAN    "\x1b[36m"
#define RESET   "\x1b[0m"

#define BOLD    "\x1b[1m"
#define DIM     "\x1b[2m"
#define UNDER   "\x1b[4m"

static inline void print_format_start(const char* style, const char* color) {
	printf("%s", style);
	printf("%s", color);
}
static inline void print_format_end() {
	printf(RESET);
}

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
    fflush(stdout);

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

#define TAG_DEBUG "[DEBUG ] "
static inline void _debug_caller(const char *fmt, ...)
{
    char buf[1024];

    va_list args;
    va_start(args, fmt);
    int len = format_log(
        buf, sizeof(buf),
        TAG_DEBUG,
        COLOR_DEBUG,
        fmt,
        args
    );
    va_end(args);

    if (len > 0)
        write_log(2, buf, (size_t)len);
}

#define TAG_INFO   "[ INFO ] "

static inline void _info_caller(const char *fmt, ...)
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

#define TAG_WARN "[ WARN ] "
static inline void _warn_caller(const char *fmt, ...)
{
    char buf[1024];

    va_list args;
    va_start(args, fmt);
    int len = format_log(
        buf, sizeof(buf),
        TAG_WARN, 
        COLOR_WARN,
        fmt,
        args
    );
    va_end(args);

    if (len > 0)
        write_log(2, buf, (size_t)len);
}
#define TAG_ERR   "[ERROR ] "

static inline void _err_caller(const char *fmt, ...)
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
#define TAG_PANIC "[PANIC ] "
static inline void _panic_caller(const char *fmt, ...)
{
    char buf[1024];

    va_list args;
    va_start(args, fmt);
    int len = format_log(
        buf, sizeof(buf),
        TAG_PANIC, 
        COLOR_ERROR,
        fmt,
        args
    );
    va_end(args);

    if (len > 0)
        write_log(2, buf, (size_t)len);
    assert(0);
}
static inline void _null_caller(const char *fmt, ...) {}
#define LL_NONE  0
#define LL_ERR   1
#define LL_WARN  2
#define LL_INFO  3
#define LL_DBG   4

#define LOG_LEVEL LL_NONE
#ifndef LOG_LEVEL
#define LOG_LEVEL LL_DBG   // default
#endif



#define panic(fmt, ...)  _panic_caller(fmt, ##__VA_ARGS__)
#if LOG_LEVEL >= LL_DBG
#define dbg(fmt, ...)  _debug_caller(fmt, ##__VA_ARGS__)
#else
#define dbg(fmt, ...)   _null_caller(fmt, ##__VA_ARGS__)
#endif

#if LOG_LEVEL >= LL_INFO
#define info(fmt, ...) _info_caller(fmt, ##__VA_ARGS__)
#else
#define info(fmt, ...)   _null_caller(fmt, ##__VA_ARGS__)
#endif

#if LOG_LEVEL >= LL_WARN
#define warn(fmt, ...) _warn_caller(fmt, ##__VA_ARGS__)
#else
#define warn(fmt, ...)   _null_caller(fmt, ##__VA_ARGS__)
#endif

#if LOG_LEVEL >= LL_ERR
#define err(fmt, ...)  _err_caller(fmt, ##__VA_ARGS__)
#else
#define err(fmt, ...)   _null_caller(fmt, ##__VA_ARGS__)
#endif


#endif // LOGGER_H
