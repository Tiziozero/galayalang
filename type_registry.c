#include "parser.h"

#define TYPE_REGISTRY_CAP 4096
static Type* type_registry[TYPE_REGISTRY_CAP];
static int   type_registry_count = 0;

void type_registry_add(Type* t) {
    if (!t) return;
    for (int i = 0; i < type_registry_count; i++)
        if (type_registry[i] == t) return; // already registered
    if (type_registry_count >= TYPE_REGISTRY_CAP) {
        panic("Type registry full.");
        return;
    }
    type_registry[type_registry_count++] = t;
}

int type_registry_contains(Type* t) {
    for (int i = 0; i < type_registry_count; i++)
        if (type_registry[i] == t) return 1;
    return 0;
}
