#ifndef HASHMAP_H
#define HASHMAP_H

#include "utils.h"
#include "stdint.h"

#define HASHMAP_INITIAL_CAP 16
#define HASHMAP_LOAD_FACTOR 0.75

typedef struct HashEntry {
    Span key;
    void* value;
    struct HashEntry* next; // chaining for collisions
} HashEntry;

typedef struct {
    HashEntry** buckets;
    int count;
    int cap;
    Arena* arena; // borrows an arena for entry allocation
} HashMap;

static inline uint32_t _hash_span(Span key) {
    // FNV-1a
    uint32_t hash = 2166136261u;
    for (int i = 0; i < key.length; i++) {
        hash ^= (uint8_t)key.name[i];
        hash *= 16777619u;
    }
    return hash;
}

static inline HashMap hashmap_new(Arena* arena) {
    HashMap m;
    m.count = 0;
    m.cap   = HASHMAP_INITIAL_CAP;
    m.arena = arena;
    m.buckets = (HashEntry**)calloc(m.cap, sizeof(HashEntry*));
    if (!m.buckets) {
        err("hashmap_new: failed to allocate buckets.");
        assert(0);
    }
    return m;
}

// Forward declare for use in _hashmap_resize
static inline void hashmap_set(HashMap* m, Span key, void* value);

static inline void _hashmap_resize(HashMap* m) {
    int old_cap         = m->cap;
    HashEntry** old_bkt = m->buckets;

    m->cap     *= 2;
    m->count    = 0;
    m->buckets  = (HashEntry**)calloc(m->cap, sizeof(HashEntry*));
    if (!m->buckets) {
        err("_hashmap_resize: failed to allocate buckets.");
        assert(0);
    }

    for (int i = 0; i < old_cap; i++) {
        HashEntry* e = old_bkt[i];
        while (e) {
            hashmap_set(m, e->key, e->value);
            e = e->next;
        }
    }
    // old entries were arena-allocated, no need to free them individually
    free(old_bkt);
}

static inline void hashmap_set(HashMap* m, Span key, void* value) {
    assert(is_valid_name(key));
    if ((float)m->count / (float)m->cap >= HASHMAP_LOAD_FACTOR) {
        _hashmap_resize(m);
    }

    uint32_t idx = _hash_span(key) % (uint32_t)m->cap;
    HashEntry* e = m->buckets[idx];

    // Update existing key
    while (e) {
        if (name_cmp(e->key, key)) {
            e->value = value;
            return;
        }
        e = e->next;
    }

    // Insert new entry via arena
    HashEntry* entry = (HashEntry*)arena_alloc(m->arena, sizeof(HashEntry));
    entry->key   = key;
    entry->value = value;
    entry->next  = m->buckets[idx];
    m->buckets[idx] = entry;
    m->count++;
}

// Returns NULL if not found
static inline void* hashmap_get(HashMap* m, Span key) {
    assert(is_valid_name(key));
    uint32_t idx = _hash_span(key) % (uint32_t)m->cap;
    HashEntry* e = m->buckets[idx];
    while (e) {
        if (name_cmp(e->key, key)) return e->value;
        e = e->next;
    }
    return NULL;
}

// Only frees the bucket array; entries live in the arena
static inline void hashmap_free(HashMap* m) {
    free(m->buckets);
    m->buckets = NULL;
    m->count   = 0;
    m->cap     = 0;
}

#endif // HASHMAP_H
