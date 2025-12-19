#ifndef UTILS_H
#define UTILS_H
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
typedef struct {
    char* name;
    size_t length;
} Name;

static inline Name new_name(char* name, size_t length) {
    return (Name){.name=name,.length=length};
}
// returns 1 if equal, 0 if not
static inline int name_cmp(Name n1, Name n2) {
    if (n1.length != n2.length) return 0;

    for (size_t i = 0; i < n1.length; i++) {
        if (n1.name[i] != n2.name[i]) return 0;
    }
    return 1;
}

static inline void write_name(Name name) {
    fwrite(name.name, 1, name.length, stdout);
    fflush(stdout);
}

static inline void print_name(Name name) {
    write_name(name);
    printf("\n");
}

typedef struct {
    void* memory;
    size_t offset;
    size_t capacity;
} Arena;

static inline Arena arena_new(size_t capacity) {
    Arena a;
    a.memory = malloc(capacity);
    if (!a.memory) {
        fprintf(stderr, "failed to allcate memory for arena.\n");
    }

    a.capacity = capacity;
    a.offset = 0;
    return a;
}

static inline void* arena_add(Arena* a, size_t size, void* data) {
    if (a->offset + size  > a->capacity) {
        a->capacity*=2;
        a->memory = realloc(a->memory, a->capacity);
        if (!a->memory) return NULL;
    }
    void* retval =(char*) a->memory + a->offset;
    memcpy((char*)a->memory + a->offset, data, size);
    a->offset += size;
    return retval;
}

#endif // UTILS_C
//
