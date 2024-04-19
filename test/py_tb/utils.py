def print_mem_diff(real, expected):
    for idx in range(len(real)):
        if real[idx] != expected[idx]:
            print("id:", idx,
                  "expected: ", hex(expected[idx]),
                  "real: ", hex(real[idx])
                  )
