#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

uint32_t isPowerOfTwo(int num) {
  if (num <= 0) {
    return 0;
  }

  return (num & (num - 1)) == 0;
}

uint64_t c_createBRAM(uint32_t word_width, uint64_t memory_size) {

  FILE *fp = NULL;
  char buff[200];
  char hex_str[20];
  uint32_t word_idx = 0;
  int i, j;

  if (word_width < 8 || !isPowerOfTwo(word_width)) {
    fprintf(stderr, "ERROR: word must be multi of 8 bits\n");
    return 0;
  }
  uint64_t size = memory_size * (word_width / 8);
  uint32_t *p = (uint32_t *)malloc(size); // should be word-aligned

  if (p == NULL) {
    fprintf(stderr, "ERROR: fail to malloc %lldB\n", (long long)size);
    return 0;
  }

  fprintf(stdout, "create controlled BRAM with %lldB\n", (long long)size);

  fp = fopen("test_host_memory.hex", "rt");
  if (fp == NULL) {
    fprintf(stderr, "ERROR: fail to open test_host_memory.hex\n");
    return 0;
  }
  // read in each line: 16 words, each word 8 chars
  while (fgets(buff, 200, fp) != NULL) {
    for (i = 15; i >= 0; i--) {
      // copy string to another hex_str
      for (j = 0; j < 8; j++) {
        hex_str[j] = buff[i * 8 + j];
      }
      hex_str[8] = '\0';
      // read from it
      sscanf(hex_str, "%x", &(p[word_idx]));
      word_idx++;
    }
  }
  // close file
  fclose(fp);

  return (uint64_t)p;
}

void c_readBRAM(unsigned int *resultptr, uint64_t ptr, uint64_t wordAddr,
                uint32_t word_width) {
  uint8_t *mem = (uint8_t *)ptr;
  uint8_t *dst_mem = (uint8_t *)resultptr;
  ssize_t byte_cnt_per_word = word_width / 8;

  ssize_t byte_pos = wordAddr * byte_cnt_per_word;
  for (ssize_t byte_idx = 0; byte_idx < byte_cnt_per_word; byte_idx++) {
    dst_mem[byte_idx] = mem[byte_pos];
    byte_pos++;
  }
}

void c_writeBRAM(uint64_t ptr, uint64_t wordAddr, uint32_t *data,
                 uint32_t *byte_en, uint32_t word_width) {
  uint8_t *mem = (uint8_t *)ptr;
  uint8_t *data_mem = (uint8_t *)data;
  ssize_t byte_cnt_per_word = word_width / 8;

  ssize_t byte_pos = wordAddr * byte_cnt_per_word;
  for (ssize_t byte_idx = 0; byte_idx < byte_cnt_per_word; byte_idx++) {
    ssize_t byte_en_group_idx = byte_idx / 32;
    ssize_t byte_en_group_off = byte_idx & 0x1f;
    if (((byte_en[byte_en_group_idx] >> byte_en_group_off) & 0x01) == 0x01) {
      mem[byte_pos] = data_mem[byte_idx];
    }
    byte_pos++;
  }
}