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

double fadd(double a, double b) { return a + b; }

double neg(double x) { return -x; }

double half(void) { return 0.5; }

int64_t ftrunc(double x) { return (int64_t)x; }

/* Mixed arguments use separate GP and XMM counters. */
double scale(int64_t n, double x, int64_t m) { return (double)(n - m) * x; }

/* 8 XMM arguments, then 1 on the machine stack. */
double fdigits9(double a, double b, double c, double d, double e, double f,
                double g, double h, double i) {
    return (((((((a * 10 + b) * 10 + c) * 10 + d) * 10 + e) * 10 + f) * 10 +
             g) * 10 + h) * 10 + i;
}

/* 1 XMM and 6 GP arguments, then 1 on the machine stack. */
int64_t fdigits8(double a, int64_t b, int64_t c, int64_t d, int64_t e,
                 int64_t f, int64_t g, int64_t h) {
    return (int64_t)a * 10000000 + digits7(b, c, d, e, f, g, h);
}

/* Alternate 9 int and 9 float arguments. i7, i8, i9 and d9 go on the
   machine stack in this order. */
int64_t interleave(int64_t i1, double d1, int64_t i2, double d2, int64_t i3,
                   double d3, int64_t i4, double d4, int64_t i5, double d5,
                   int64_t i6, double d6, int64_t i7, double d7, int64_t i8,
                   double d8, int64_t i9, double d9) {
    int64_t v[18] = {i1, (int64_t)d1, i2, (int64_t)d2, i3, (int64_t)d3,
                     i4, (int64_t)d4, i5, (int64_t)d5, i6, (int64_t)d6,
                     i7, (int64_t)d7, i8, (int64_t)d8, i9, (int64_t)d9};
    int64_t r = 0;
    for (int k = 0; k < 18; k++) r = r * 10 + v[k];
    return r;
}

/* 0 when rsp was 16 byte aligned at the call. */
__attribute__((naked)) int64_t misaligned(void) {
    __asm__("lea 8(%rsp), %rax\n\t"
            "and $15, %rax\n\t"
            "ret");
}

/* misaligned with a float, 7 ints and 1 stack slot: the copy path. */
__attribute__((naked)) int64_t misaligned_f7(double a, int64_t b, int64_t c,
                                             int64_t d, int64_t e, int64_t f,
                                             int64_t g, int64_t h) {
    __asm__("lea 8(%rsp), %rax\n\t"
            "and $15, %rax\n\t"
            "ret");
}

/* misaligned with 9 floats: 1 stack slot stays in the frame. */
__attribute__((naked)) int64_t misaligned_f9(double a, double b, double c,
                                             double d, double e, double f,
                                             double g, double h, double i) {
    __asm__("lea 8(%rsp), %rax\n\t"
            "and $15, %rax\n\t"
            "ret");
}

/* misaligned with 7 ints and 9 floats: 2 stack slots, copied. */
__attribute__((naked)) int64_t misaligned_mix(
    int64_t a, double b, int64_t c, double d, int64_t e, double f, int64_t g,
    double h, int64_t i, double j, int64_t k, double l, int64_t m, double n,
    double o, double p) {
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
