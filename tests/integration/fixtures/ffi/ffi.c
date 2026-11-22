#include <stdint.h>

int64_t add(int64_t a, int64_t b) { return a + b; }

int64_t sub(int64_t a, int64_t b) { return a - b; }

int64_t hi32(int64_t x) { return x >> 32; }

int64_t digits7(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e,
                int64_t f, int64_t g) {
    return (((((a * 10 + b) * 10 + c) * 10 + d) * 10 + e) * 10 + f) * 10 + g;
}

int64_t digits8(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e,
                int64_t f, int64_t g, int64_t h) {
    return digits7(a, b, c, d, e, f, g) * 10 + h;
}

static int64_t saved;

void save(int64_t v) { saved = v; }

int64_t saved_value(void) { return saved; }

static int64_t cells[2] = {7, 9};

int64_t *cell(void) { return cells; }

int64_t *next(int64_t *p) { return p + 1; }

int64_t load(int64_t *p) { return *p; }

void store(int64_t *p, int64_t v) { *p = v; }

/* Stack bool is int64_t 0 or 1. */
int64_t is_neg(int64_t x) { return x < 0; }

int64_t negate(int64_t b) { return !b; }

/* 0 when rsp was 16 byte aligned at the call. */
__attribute__((naked)) int64_t misaligned(void) {
    __asm__("lea 8(%rsp), %rax\n\t"
            "and $15, %rax\n\t"
            "ret");
}

/* Change all caller-saved integer registers. */
__attribute__((naked)) void clobber(void) {
    __asm__("mov $-1, %rax\n\t"
            "mov %rax, %rcx\n\t"
            "mov %rax, %rdx\n\t"
            "mov %rax, %rsi\n\t"
            "mov %rax, %rdi\n\t"
            "mov %rax, %r8\n\t"
            "mov %rax, %r9\n\t"
            "mov %rax, %r10\n\t"
            "mov %rax, %r11\n\t"
            "ret");
}
