/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   Eliminated concurency from C program contributed by Petr Prokhorenkov
   Modified by Leo Osvald
*/

#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//#include "simple_hash3.h"

#define HT_SIZE 2000000

typedef unsigned char uint8_t;
const uint8_t selector[] = { -1, 0,-1, 1, 3,-1,-1, 2 };
const char table[] = {'A', 'C', 'G', 'T'};

char * read_input(int *input_size, const char* input_path) {
  FILE *input = fopen(input_path, "r");
  fseek(input, 0L, SEEK_END);
  long file_size = ftell(input);
  fseek(input, 0L, SEEK_SET);

  char *result = malloc(file_size);

  do {
    fgets_unlocked(result, file_size, input);
  } while (strncmp(result, ">THREE", 6));

  int read = 0;
  while (fgets_unlocked(result + read, file_size - read, input)) {
    int len = strlen(result + read);
    if (len == 0 || result[read] == '>')
      break;
    read += len;
    if (result[read - 1] == '\n')
      read--;
  }

  result[read++] = '>';
  result = realloc(result, read);
  *input_size = read;

  return result;
}

static inline const char * next_char(const char *p) {
  do {
    ++p;
  } while (isspace(*p));

  return p;
}

inline uint64_t push_char(uint64_t cur, uint8_t c) {
  return (cur << 2) + selector[(c & 7)];
}

uint64_t pack_key(const char *key, int len) {
  uint64_t code = 0;
  for (int i = 0; i < len; i++) {
    code = push_char(code, *key);
    key = next_char(key);
  }

  return code;
}

void unpack_key(uint64_t key, int length, char *buffer) {
  for (int i = length - 1; i > -1; i--) {
    buffer[i] = table[key & 3];
    key >>= 2;
  }
  buffer[length] = 0;
}

void generate_seqences(char *start, int length, int frame, struct ht_ht *ht) {
  uint64_t code = 0;
  const uint64_t mask = (1ull << 2*frame) - 1;
  char *p = start;
  char *end = start + length;

  // Pull first frame.

  for (int i = 0; i < frame; i++) {
    code = push_char(code, *p);
    ++p;
  }
  ht_find_new(ht, code)->val++;

  while (p < end) {
    code = push_char(code, *p) & mask;
    ht_find_new(ht, code)->val++;
    ++p;
    if (*p & 8) {
      if (*p & 1) {
	++p;
      } else
	break;
    }
  }
}

int key_count_cmp(const void *l, const void *r) {
  const struct ht_node *lhs = l, *rhs = r;

  if (lhs->val != rhs->val)
    return rhs->val - lhs->val;

  // Overflow is possible here,
  // so use comparisons instead of subtraction.
  if (lhs->key < rhs->key)
    return -1;
  if (lhs->key > rhs->key)
    return 1;
  return 0;
}

struct print_freqs_param {
  char *start;
  int length;
  int frame;
};

struct ht_node * ht_values_as_vector(struct ht_ht *ht) {
  struct ht_node *v = malloc(ht->items*sizeof(struct ht_node));
  struct ht_node *n = ht_first(ht);

  for (int i = 0; i < ht->items; i++) {
    v[i] = *n;
    n = ht_next(ht);
  }

  return v;
}

void print_freqs(struct print_freqs_param *param) {
  char *start = param->start;
  int length = param->length;
  int frame = param->frame;

  struct ht_ht *ht = ht_create(32);
  char buffer[frame + 1];

  generate_seqences(start, length, frame, ht);
    
  struct ht_node *counts = ht_values_as_vector(ht);
  int size = ht->items;

  qsort(counts, size, sizeof(struct ht_node), &key_count_cmp);

  int total_count = 0;
  for (int i = 0; i < size; i++) {
    total_count += counts[i].val;
  }

  for (int i = 0; i < size; i++) {
    unpack_key(counts[i].key, frame, buffer);
    printf("%s %.3f\n", buffer, counts[i].val * 100.0f / total_count);
  }

  free(counts);
  ht_destroy(ht);
}

struct print_occurences_param {
  char *start;
  int length;
  char *nuc_seq;
};

void print_occurences(struct print_occurences_param *param) {
  char *start = param->start;
  int length = param->length;
  char *nuc_seq = param->nuc_seq;
  int nuc_seq_len = strlen(nuc_seq);
  struct ht_ht *ht = ht_create(HT_SIZE);

  generate_seqences(start, length, nuc_seq_len, ht);

  uint64_t key = pack_key(nuc_seq, nuc_seq_len);
  int count = ht_find_new(ht, key)->val;
  printf("%d\t%s\n", count, nuc_seq);
    
  ht_destroy(ht);
}

int main(int argc, char **argv) {
  int input_size;
  char *input_mem = read_input(&input_size, argv[1]);

#   define DECLARE_PARAM(o, n) {		\
    .start = input_mem,				\
    .length = input_size,			\
    .frame = n }

  struct print_freqs_param freq_params[2] = {
    DECLARE_PARAM(0, 1),
    DECLARE_PARAM(1, 2)
  }; 
#   undef DECLARE_PARAM

  for (int i = 0; i < 2; i++)
    print_freqs(freq_params + i);

#   define DECLARE_PARAM(o, s) {		\
    .start = input_mem,				\
    .length = input_size,			\
    .nuc_seq = s }

  struct print_occurences_param occurences_params[5] = {
    DECLARE_PARAM(2, "GGT"),
    DECLARE_PARAM(3, "GGTA"),
    DECLARE_PARAM(4, "GGTATT"),
    DECLARE_PARAM(5, "GGTATTTTAATT"),
    DECLARE_PARAM(6, "GGTATTTTAATTTATAGT")
  };
#   undef DECLARE_PARAM

  for (int i = 0; i < 5; i++)
    print_occurences(occurences_params + i);

  free(input_mem);

  return 0;
}
