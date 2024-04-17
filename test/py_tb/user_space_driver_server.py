import socket
import json
import threading


import mock_host

class UserspaceDriverServer:
    def __init__(self, listen_addr, listen_port_a, listen_port_b, share_mem_path, share_mem_size) -> None:
        self.listen_addr = listen_addr
        self.driver_listen_port_a = listen_port_a
        self.simulator_listen_port_a = listen_port_a + 1

        self.driver_listen_port_b = listen_port_b
        self.simulator_listen_port_b = listen_port_b + 1

        self.host_mem = mock_host.MockHostMem(share_mem_path, share_mem_size)
        self.mock_nic_a = mock_host.MockNicAndHost(
            self.host_mem, host=self.listen_addr, port=self.simulator_listen_port_a, rx_packet_wait_time=0)
        self.mock_nic_b = mock_host.MockNicAndHost(
            self.host_mem, host=self.listen_addr, port=self.simulator_listen_port_b, rx_packet_wait_time=0.001)

    def run(self):
        mock_host.MockNicAndHost.connect_two_card(
            self.mock_nic_a, self.mock_nic_b)
        self.mock_nic_a.run()
        self.mock_nic_b.run()

        self.stop_flag = False

        self.server_thread_a = threading.Thread(target=self._run, args=(
            self.listen_addr, self.driver_listen_port_a, self.mock_nic_a))
        self.server_thread_a.start()

        self.server_thread_b = threading.Thread(target=self._run, args=(
            self.listen_addr, self.driver_listen_port_b, self.mock_nic_b))
        self.server_thread_b.start()

    def stop(self):
        self.mock_nic.stop()
        self.stop_flag = True

    def _run(self, listen_addr, listen_port, mock_nic):
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind((listen_addr, listen_port))

        while not self.stop_flag:

            recv_raw, resp_addr = server_socket.recvfrom(1024)
            recv_req = json.loads(recv_raw)

            if recv_req["is_write"]:
                mock_nic.write_csr_blocking(
                    recv_req["addr"], recv_req["value"])
            else:
                value = mock_nic.read_csr_blocking(recv_req["addr"])
                server_socket.sendto(json.dumps(
                    {"value": value, "addr": recv_req["addr"], "is_write": False}).encode("utf-8"), resp_addr)


if __name__ == "__main__":
    server = UserspaceDriverServer(
        "0.0.0.0", 9873, 9875, "/bluesim1", 1024*1024*64)
    server.run()
