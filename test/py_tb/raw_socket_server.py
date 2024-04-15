from user_space_driver_server import UserspaceDriverServer, mock_host

if __name__ == "__main__":
    host_mem = mock_host.MockHostMem("/bluesim1", 1024 * 1024 * 64)
    listen_addr = "0.0.0.0"
    listen_driver_port_a = 9873
    listen_simulator_port_a = listen_driver_port_a + 1
    mock_nic_a = mock_host.EmulatorMockNicAndHost(
        host_mem,
        host=listen_addr,
        port=listen_simulator_port_a,
        rx_packet_wait_time=0)
    mock_nic_b = mock_host.RawsocketMockNicAndHost(
        "127.0.0.2", 4791, "ab:ab:ab:ab:ab:ab", "cd:cd:cd:cd:cd:cd")
    server = UserspaceDriverServer(
        listen_addr,
        mock_nic_a,
        listen_driver_port_a,
        mock_nic_b,
        None)
    server.run()
