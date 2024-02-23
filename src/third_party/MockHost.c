#include <arpa/inet.h>
#include <netinet/in.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#define BUFFER_SIZE 1024

int rpc_socket_fd = 0;
pthread_mutex_t mutex;

int connect_to_server(const char *ip_addr, uint16_t port) {
  int sock = 0;
  struct sockaddr_in serv_addr;

  if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    printf("\n Socket creation error \n");
    exit(EXIT_FAILURE);
  }

  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(port);

  if (inet_pton(AF_INET, ip_addr, &serv_addr.sin_addr) <= 0) {
    printf("\nInvalid address/ Address not supported \n");
    exit(EXIT_FAILURE);
  }

  if (connect(sock, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) {
    printf("\nConnection Failed \n");
    exit(EXIT_FAILURE);
  }

  return sock;
}

uint32_t isPowerOfTwo(int num) {
  if (num <= 0) {
    return 0;
  }

  return (num & (num - 1)) == 0;
}

uint64_t c_createBRAM(uint32_t word_width, uint64_t memory_size) {
  if (word_width < 8 || !isPowerOfTwo(word_width)) {
    fprintf(stderr, "ERROR: word must be multi of 8 bits\n");
    exit(EXIT_FAILURE);
  }
  uint64_t size = memory_size * (word_width / 8);
  uint32_t *p = (uint32_t *)malloc(size); // should be word-aligned

  if (p == NULL) {
    fprintf(stderr, "ERROR: fail to malloc %lldB\n", (long long)size);
    exit(EXIT_FAILURE);
  }

  fprintf(stdout, "create controlled BRAM with %lldB\n", (long long)size);

  rpc_socket_fd = connect_to_server("0.0.0.0", 9876);
  pthread_mutex_init(&mutex, NULL);

  return (uint64_t)p;
}

void do_rpc(void *req_data, ssize_t req_len, void *resp_data,
            ssize_t resp_len) {

  pthread_mutex_lock(&mutex);
  send(rpc_socket_fd, req_data, req_len, 0);

  uint8_t *dst_ptr = (uint8_t *)resp_data;
  ssize_t read_cnt;
  while (resp_len > 0) {
    read_cnt = read(rpc_socket_fd, dst_ptr, resp_len);
    if (read_cnt == 0) {
      fprintf(stderr, "ERROR: bluesim connection to python broken\n");
      exit(EXIT_FAILURE);
    }
    dst_ptr += read_cnt;
    resp_len -= read_cnt;
  }
  pthread_mutex_unlock(&mutex);
}

typedef enum {
  RpcOpcodePcieBarGetReadReq = 1,
  RpcOpcodePcieBarPutReadResp = 2,
  RpcOpcodePcieBarGetWriteReq = 3,
  RpcOpcodePcieBarPutWriteResp = 4,
  RpcOpcodePcieMemWrite = 5,
  RpcOpcodePcieMemRead = 6

} RpcOpcode;

typedef struct {
  RpcOpcode opcode;
  uint64_t client_id;
  uint64_t tag;
} RpcHeader;

typedef struct {
  uint64_t value;
  uint64_t addr;
  uint64_t valid;
  uint64_t pci_tag;
} BarIoInfo;

typedef struct {
  RpcHeader header;
  BarIoInfo payload;
} RpcPcieBarAccessMessage;

typedef struct {
  uint64_t word_addr;
  uint64_t word_width;
  uint8_t data[64];
  uint8_t byte_en[8];
} MemoryIoInfo;

typedef struct {
  RpcHeader header;
  MemoryIoInfo payload;
} RpcPcieMemoryAccessMessage;

void do_pcie_bar_get_request(BarIoInfo *resultptr, uint64_t client_id,
                             uint8_t is_read) {
  RpcPcieBarAccessMessage req;
  RpcPcieBarAccessMessage resp;
  req.header.opcode =
      is_read ? RpcOpcodePcieBarGetReadReq : RpcOpcodePcieBarGetWriteReq;
  req.header.client_id = client_id;
  do_rpc(&req, sizeof(req), &resp, sizeof(resp));
  memcpy(resultptr, &resp.payload, sizeof(resp.payload));
}

void c_getPcieBarReadReq(BarIoInfo *resultptr, uint64_t client_id) {
  do_pcie_bar_get_request(resultptr, client_id, 1);
}

void c_getPcieBarWriteReq(BarIoInfo *resultptr, uint64_t client_id) {
  do_pcie_bar_get_request(resultptr, client_id, 0);
}

void do_pcie_bar_put_response(uint64_t client_id, BarIoInfo *result,
                              uint8_t is_read) {
  RpcPcieBarAccessMessage msg;
  msg.header.opcode =
      is_read ? RpcOpcodePcieBarPutReadResp : RpcOpcodePcieBarPutWriteResp;
  msg.header.client_id = client_id;
  memcpy(&msg.payload, result, sizeof(msg.payload));
  do_rpc(&msg, sizeof(msg), NULL, 0);
}

void c_putPcieBarReadResp(uint64_t client_id, BarIoInfo *result) {
  do_pcie_bar_put_response(client_id, result, 1);
}

void c_putPcieBarWriteResp(uint64_t client_id, BarIoInfo *result) {
  do_pcie_bar_put_response(client_id, result, 0);
}

void c_readBRAM(unsigned int *resultptr, uint64_t client_id, uint64_t wordAddr,
                uint32_t word_width) {
  RpcPcieMemoryAccessMessage req_msg;
  RpcPcieMemoryAccessMessage resp_msg;

  req_msg.header.opcode = RpcOpcodePcieMemRead;
  req_msg.header.client_id = client_id;
  req_msg.payload.word_addr = wordAddr;
  req_msg.payload.word_width = word_width;
  do_rpc(&req_msg, sizeof(req_msg), &resp_msg, sizeof(resp_msg));

  ssize_t byte_cnt_per_word = word_width / 8;
  memcpy((uint8_t *)resultptr, resp_msg.payload.data, byte_cnt_per_word);
}

void c_writeBRAM(uint64_t client_id, uint64_t wordAddr, uint32_t *data,
                 uint32_t *byte_en, uint32_t word_width) {
  uint8_t *data_mem = (uint8_t *)data;
  ssize_t byte_cnt_per_word = word_width / 8;

  RpcPcieMemoryAccessMessage req_msg;
  req_msg.header.opcode = RpcOpcodePcieMemWrite;
  req_msg.header.client_id = client_id;
  req_msg.payload.word_addr = wordAddr;
  req_msg.payload.word_width = word_width;
  memcpy(req_msg.payload.byte_en, byte_en, byte_cnt_per_word / 8);
  memcpy(req_msg.payload.data, data, byte_cnt_per_word);
  do_rpc(&req_msg, sizeof(req_msg), NULL, 0);
}