#include "main.h"
#include <string.h>

void Main_destruct(Main_t* self) {
    (void)self;
}

void Main_run(Provider_t* a) {
    const char* print = "Hello, world!\n";
    size_t len = strlen(print);
    for (size_t i = 0; i < len; i++) {
        a->_siders_vtable->print(a, print[i]);
    }
    a->_siders_vtable->destruct(a);
}
