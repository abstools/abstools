#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <gmp.h>

// String representation

typedef struct absstr {
    size_t size;
    char *data;
} absstr;

int absstr_alloc(absstr *str, size_t size) {
    str->data = malloc(size+1);
    str->data[size] = 0;
    str->size = size;
    return 0;
}

int absstr_deinit(absstr *str) {
    free(str->data);
    str->data = 0;
    str->size = 0;
    return 0;
}

void absstr_print(absstr str) {
    fwrite(str.data, 1, str.size, stdout);
}

void absstr_println(absstr str) {
    fwrite(str.data, 1, str.size, stdout);
    fwrite("\n", 1, 1, stdout);
    fflush(stdout);
}

void absstr_literal(absstr *str, char *data, size_t size) {
    absstr_alloc(str, size);
    memcpy(str->data, data, size);
}

void absstr_concat(absstr *result, absstr left, absstr right) {
    absstr_alloc(result, left.size + right.size);
    memcpy(result->data, left.data, left.size);
    memcpy(&result->data[left.size], right.data, right.size);
}

// Integer representation
typedef struct absint {
    mpz_t mpint;
} absint;

void absint_initzero(absint *val) {
    mpz_init(val->mpint);
}

void absint_initcopy(absint *val, absint other) {
    mpz_init_set(val->mpint, other.mpint);
}

void absint_deinit(absint *val) {
    mpz_clear(val->mpint);
}

void absint_literal(absint *val, char *data, size_t size) {
    mpz_init_set_str(val->mpint, data, 10);
}

int absint_compare(absint left, absint right) {
    return mpz_cmp(left.mpint, left.mpint);
}

void absint_add(absint *val, absint left, absint right) {
    mpz_init(val->mpint);
    mpz_add(val->mpint, left.mpint, right.mpint);
}

void absint_sub(absint *val, absint left, absint right) {
    mpz_init(val->mpint);
    mpz_sub(val->mpint, left.mpint, right.mpint);
}

void absint_mul(absint *val, absint left, absint right) {
    mpz_init(val->mpint);
    mpz_mul(val->mpint, left.mpint, right.mpint);
}

void absint_div(absint *val, absint left, absint right) {
    mpz_init(val->mpint);
    mpz_div(val->mpint, left.mpint, right.mpint);
}

void absint_mod(absint *val, absint left, absint right) {
    mpz_init(val->mpint);
    mpz_mod(val->mpint, left.mpint, right.mpint);
}
