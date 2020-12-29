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

void absstr_tostring(absstr *result, absstr str) {
    absstr_alloc(result, str.size);
    memcpy(result->data, str.data, str.size);
}

int absstr_compare(absstr a, absstr b) {
    for (size_t i = 0;; i++) {

        // We reached the end of both strings at the same time
        if (i >= a.size && i >= b.size) return 0;

        // We reached the end of the RHS string => the LHS is greater.
        if (i >= b.size) return 1;

        // We reached the end of the LHS string => the RHS is greater.
        if (i >= a.size) return -1;

        int diff = a.data[i] - b.data[i];
        if (diff != 0) return diff;
    }
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

void absint_tostring(absstr *result, absint val) {
    result->data = mpz_get_str(NULL, 10, val.mpint);
    result->size = strlen(result->data);
}

int absint_compare(absint left, absint right) {
    return mpz_cmp(left.mpint, right.mpint);
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
