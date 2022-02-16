#include "main.h"
#include <string.h>

void Main_destruct(Main_t* self) {
    (void)self;
}

static Provider_t* instance = 0;

void Main_set(Provider_t* a) {
    instance = a;
}

void Main_run() {
    Provider_t* a = instance;
    instance = 0;

    const char* print = "Hello, world!\n";
    size_t len = strlen(print);
    for (size_t i = 0; i < len; i++) {
        a->_siders_vtable->print(a, print[i]);
    }
    a->_siders_vtable->destruct(a);
}
