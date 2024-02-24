import socket
import json
import threading

import mock_host


class UserspaceDriverServer:
    def __init__(self, listen_addr, listen_port, share_mem_path, share_mem_size) -> None:
        self.listen_addr = listen_addr
        self.listen_port = listen_port
        self.host_mem = mock_host.MockHostMem(share_mem_path, share_mem_size)
        self.mock_nic = mock_host.MockNicAndHost(self.host_mem)

    def run(self):
        self.mock_nic.run()
        self.stop_flag = False
        self.server_thread = threading.Thread(target=self._run)
        self.server_thread.start()

    def stop(self):
        self.mock_nic.stop()
        self.stop_flag = True

    def _run(self):
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind((self.listen_addr, self.listen_port))
        server_socket.listen(1)

        while not self.stop_flag:
            client_socket, client_address = server_socket.accept()
            client_socket.settimeout(0.1)
            print('User Space Driver Client connected:', client_address)

            while not self.stop_flag:

                recv_raw = client_socket.recv()
                recv_req = json.loads(recv_raw)

                if recv_req["is_write"]:
                    self.mock_nic.write_csr_blocking(
                        recv_req["addr"], recv_req["value"])
                    client_socket.send(json.dumps({"value": 0}))
                else:
                    value = self.mock_nic.read_csr_blocking(recv_req["addr"])
                    client_socket.send(json.dumps({"value": value}))


if __name__ == "__main__":
    server = UserspaceDriverServer("0.0.0.0", 9875, "/bluesim1", 1024*1024*64)
    server.run()
