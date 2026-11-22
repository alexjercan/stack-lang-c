// libc allocation for the runtime tests. The program has no C runtime startup;
// the dynamic loader initializes libc.
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

void *c_malloc(int64_t n) { return malloc((size_t)n); }
void *c_realloc(void *p, int64_t n) { return realloc(p, (size_t)n); }
void c_free(void *p) { free(p); }
void c_fill(void *p, int64_t n, int64_t v) { memset(p, (int)v, (size_t)n); }

// 1 when the n bytes at p are all v.
int64_t c_filled(void *p, int64_t n, int64_t v) {
    unsigned char *b = p;
    for (int64_t i = 0; i < n; i++)
        if (b[i] != (unsigned char)v)
            return 0;
    return 1;
}

int64_t c_break(void) { return (int64_t)(intptr_t)sbrk(0); }

// An address in an unmapped page between two mapped pages, or NULL.
void *c_hole(void) {
    long page = sysconf(_SC_PAGESIZE);
    char *p = mmap(NULL, 3 * page, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (p == MAP_FAILED || munmap(p + page, page) != 0)
        return NULL;
    return p + page + 24;
}
