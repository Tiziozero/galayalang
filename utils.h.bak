#ifndef UTILS_H
#define UTILS_H
#include "logger.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


#define TODO(fmt, ...)                                                   \
    do {                                                                 \
        err("TODO: %-20s:%5d | " fmt,                                    \
            __FILE__, __LINE__, ##__VA_ARGS__);                           \
        assert(0);                                                       \
    } while (0)
typedef struct {
    char* name;
    size_t length;
} Span;

static inline char* print_name_to_buf(char* buf, size_t size, Span name) {
    if (name.name == 0 || name.length == 0) assert( 0 && "invalid name");
    size_t len =  name.length < size? name.length : size;
    memcpy(buf, name.name, len);
    buf[len] = 0;

    return buf;
}

static inline int is_valid_name(Span name) {
    return name.name != 0 && name.length != 0;
}
static inline Span new_name(char* name, size_t length) {
    return (Span){.name=name,.length=length};
}
// returns 1 if equal, 0 if not
static inline int name_cmp(Span n1, Span n2) {
    if (!n1.name || !n2.name || !n1.length || !n2.length) {
        panic("somthing is 0 in namecmp %zu %zu %zu %zu.",
                n1.name, n2.name, n1.length, n2.length);
        return 0;
    }
    if (n1.length != n2.length) return 0;

    for (size_t i = 0; i < n1.length; i++) {
        if (n1.name[i] != n2.name[i]) return 0;
    }
    return 1;
}

static inline void write_name(Span name) {
    fwrite(name.name, 1, name.length, stdout);
    fflush(stdout);
}

inline static void _print_name(Span n) {
    printf("%.*s", (int)n.length, n.name);
}



typedef struct {
    void* memory;
    void** pages;
    size_t page_size;
    size_t current_page;
    size_t pages_count;
    size_t offset;
    size_t allocations;
    size_t total_allocated;
} Arena;

static inline Arena arena_new(size_t page_size, size_t item_size) {
    Arena a;
    a.pages_count = 10; // base size
    a.current_page = 0;
    a.allocations = 0;
    a.total_allocated = 0;
    a.page_size = page_size*item_size;
    a.pages = (void**)malloc(a.pages_count*sizeof(void**));
    if (!a.pages) {
        err("Failed to allocate page size of arena. shit's gone really bad.");
        assert(0);
    }
    for (size_t i = 0; i < a.pages_count; i++) {
        a.pages[i] = malloc(a.page_size);
        if (!a.pages[i] ) {
            fprintf(stderr, "failed to allcate memory for arena.\n");
            assert(0);
        }
    }
    a.memory = a.pages[a.current_page];
    a.offset = 0;
    dbg("New arena of page_size: %zu (%zu * %zu)",
         a.page_size, page_size, item_size);
    return a;
}

static inline void* arena_alloc(Arena* a, size_t size) {
    // dbg("allocation.");
    if (size > a->page_size) {
        printf("size too large %zu\n", size);
        assert(0);
        return NULL;
    }
    if (a->offset + size > a->page_size) {
        a->offset = 0;
        a->memory = a->pages[++a->current_page];
        if (a->current_page >= a->pages_count) {
            err("More pages required in arena.\n\tcurrent page:"
                "%zu\n\toffset: %zu\n\t", a->current_page, a->offset);
            err("allocations/allocated %zu/%zu.", a->allocations, a->total_allocated);
            assert(0);
        }
    }
    void* retval = (char*) a->memory + a->offset;
    a->offset += size;
    a->total_allocated += size;
    a->allocations ++;
    return retval;
}
static inline void* _arena_add(Arena* a, size_t size, void* data) {
    if (size > a->page_size) {
        err("size too large");
        assert(0);
        return NULL;
    }
    if (a->offset + size > a->page_size) {
        a->offset = 0;
        a->memory = a->pages[++a->current_page];
        if (a->current_page >= a->pages_count) {
            err("More pages required in arena.\n\tcurrent page:"
                "%zu\n\toffset: %zu\n\t", a->current_page, a->offset);
            assert(0);
        }
    }
    void* retval = (char*) a->memory + a->offset;
    memcpy(retval, data, size);
    a->offset += size;
    return retval;
}
        /*    a->pages_count *= 2;
            a->pages = (void**)malloc(a->pages_count*sizeof(void**));
            if (!a->pages) {
                err("Failed to reallocate page size of arena. shit's gone really bad.");
                assert(0);
            }
            a->offset = 0;
        } */

static char* name_to_cstr(Arena* arena, Span n) {
    char* s = (char*)arena_alloc(arena, n.length + 1);
    memcpy(s, n.name, n.length);
    s[n.length] = 0;
    return s;
}
static inline Span cstr_to_name(const char* s) {
    return (Span){.name=(char*)s,.length=strlen(s)};
}

static inline Span get_name_from_path(const char *path) {
    Span result = {0};

    if (!path) return result;

    const char *last_slash = strrchr(path, '/');
    const char *last_backslash = strrchr(path, '\\');

    const char *filename = path;

    if (last_slash && last_backslash)
        filename = (last_slash > last_backslash ? last_slash : last_backslash) + 1;
    else if (last_slash)
        filename = last_slash + 1;
    else if (last_backslash)
        filename = last_backslash + 1;

    size_t len = strlen(filename);

    const char *ext = ".gala";
    size_t ext_len = 5;

    if (len <= ext_len || strcmp(filename + len - ext_len, ext) != 0)
        return result; // not a .gala file

    result.name = (char *)filename;   // points inside path
    result.length = len - ext_len;    // exclude ".gala"

    return result;
}
#endif // UTILS_C
//
