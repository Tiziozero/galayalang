#include <stdio.h>
typedef enum {
    BLACK   = 30,
    RED     = 31,
    GREEN   = 32,
    YELLOW  = 33,
    BLUE    = 34,
    MAGENTA = 35,
    CYAN    = 36,
    WHITE   = 37
} Color;

// Optional style: 0=normal, 1=bold
inline void print_in_color(Color fg, int style) {
    printf("\033[%d;%dm", style, fg);
}

// Reset to default terminal colors
inline void stop_color() {
    printf("\033[0m");
}
// Optional style: 0=normal, 1=bold
// GPTMAXXING macro
#define FAILED(msg, ...) do {  print_in_color(RED, 1); fprintf(stdout,"\nFAILED: %4d\t" msg "\n\n", __LINE__, ##__VA_ARGS__); \
    stop_color();\
    fflush(stdout); \
    assert(1==0); \
    break; \
} while(0)
#define Info(msg, ...) do { \
    if(!_log_off) { \
        print_in_color(GREEN, 1); \
        printf("Info: "); \
        stop_color();\
        printf(msg, ##__VA_ARGS__); \
        fflush(stdout); \
        break; \
    }\
} while(0)

