#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>

/* =====================
   ARENA ALLOCATOR
   ===================== */

typedef struct {
    char *base;
    size_t capacity;
    size_t offset;
} Arena;

Arena arena_create(size_t capacity) {
    Arena a;
    a.base = malloc(capacity);
    a.capacity = capacity;
    a.offset = 0;
    return a;
}

void *arena_alloc(Arena *a, size_t size) {
    if (a->offset + size > a->capacity) return NULL;
    void *ptr = a->base + a->offset;
    a->offset += size;
    return ptr;
}

/* =====================
   KEY–VALUE STORE
   ===================== */

typedef struct {
    const char *key;
    size_t key_len;
    void *value;
} KVPair;

typedef struct {
    KVPair *items;
    size_t count;
    size_t capacity;
    Arena *arena;  // keys live here
} KVStore;

KVStore kv_create(Arena *arena, size_t initial_capacity) {
    KVStore kv;
    kv.items = calloc(initial_capacity, sizeof(KVPair));
    kv.count = 0;
    kv.capacity = initial_capacity;
    kv.arena = arena;
    return kv;
}

static void kv_grow(KVStore *kv) {
    kv->capacity *= 2;
    kv->items = realloc(kv->items, kv->capacity * sizeof(KVPair));
}

void kv_add(KVStore *kv, const char *key, size_t key_len, void *value) {
    // Resize array if needed
    if (kv->count == kv->capacity)
        kv_grow(kv);

    // Copy key into arena
    char *stored_key = arena_alloc(kv->arena, key_len);
    memcpy(stored_key, key, key_len);

    KVPair *p = &kv->items[kv->count++];
    p->key = stored_key;
    p->key_len = key_len;
    p->value = value;
}

void *kv_get(KVStore *kv, const char *key, size_t key_len) {
    for (size_t i = 0; i < kv->count; i++) {
        KVPair *p = &kv->items[i];
        if (p->key_len == key_len &&
            memcmp(p->key, key, key_len) == 0) {
            return p->value;
        }
    }
    return NULL;
}

/* =====================
   Example usage
   ===================== */

int test_main() {
    Arena a = arena_create(1024 * 1024); // 1MB arena
    KVStore kv = kv_create(&a, 4);

    int x = 10;
    int y = 20;

    kv_add(&kv, "hello", 5, &x);
    kv_add(&kv, "world", 5, &y);

    printf("%d\n", *(int*)kv_get(&kv, "hello", 5));
    printf("%d\n", *(int*)kv_get(&kv, "world", 5));

    return 0;
}

