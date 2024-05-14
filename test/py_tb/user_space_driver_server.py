import socket
import json
import threading


import mock_host


class UserspaceDriverServer:
    def __init__(self, listen_addr, nic_a, listen_port_a: int,
                 nic_b, listen_port_b: int) -> None:
        self.listen_addr = listen_addr
        if isinstance(nic_a, mock_host.EmulatorMockNicAndHost):
            self.driver_listen_port_a = listen_port_a
            self.simulator_listen_port_a = listen_port_a + 1
        if isinstance(nic_b, mock_host.EmulatorMockNicAndHost):
            self.driver_listen_port_b = listen_port_b
            self.simulator_listen_port_b = listen_port_b + 1
        self.mock_nic_a = nic_a
        self.mock_nic_b = nic_b

    def run(self):
        mock_host.NicManager.connect_two_card(
            self.mock_nic_a, self.mock_nic_b)
        self.mock_nic_a.run()
        self.mock_nic_b.run()

        self.stop_flag = False
        if isinstance(self.mock_nic_a, mock_host.EmulatorMockNicAndHost):
            self.server_thread_a = threading.Thread(target=self._run, args=(
                self.listen_addr, self.driver_listen_port_a, self.mock_nic_a))
            self.server_thread_a.start()
        if isinstance(self.mock_nic_b, mock_host.EmulatorMockNicAndHost):
            self.server_thread_b = threading.Thread(target=self._run, args=(
                self.listen_addr, self.driver_listen_port_b, self.mock_nic_b))
            self.server_thread_b.start()

    def stop(self):
        self.mock_nic_a.stop()
        self.mock_nic_b.stop()
        self.stop_flag = True

    def _run(self, listen_addr, listen_port,
             mock_nic: mock_host.EmulatorMockNicAndHost):
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
    host_mem = mock_host.MockHostMem("/bluesim1", 1024 * 1024 * 64)
    listen_addr = "0.0.0.0"
    listen_driver_port_a = 9873
    listen_simulator_port_a = listen_driver_port_a + 1
    listen_driver_port_b = 9875
    listen_simulator_port_b = listen_driver_port_b + 1
    mock_nic_a = mock_host.EmulatorMockNicAndHost(
        host_mem,
        host=listen_addr,
        port=listen_simulator_port_a)
    mock_nic_b = mock_host.EmulatorMockNicAndHost(
        host_mem,
        host=listen_addr,
        port=listen_simulator_port_b)
    server = UserspaceDriverServer(
        listen_addr,
        mock_nic_a,
        listen_driver_port_a,
        mock_nic_b,
        listen_driver_port_b)
    server.run()
