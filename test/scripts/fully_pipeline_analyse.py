# coding: utf-8

import sys
import re

time_interval = int(sys.argv[1])
filter_re = re.compile(sys.argv[2])
TIME_PREFIX = "time="

last_time_int = -1
continous_cnt = 0

continous_hist = {}
non_continous_hist = {}


sys.stdin.reconfigure(encoding='utf-8', errors="ignore")

non_continous_cnt = 0
for line in sys.stdin:
    if not filter_re.search(line):
        continue

    time_str = line.split(":", maxsplit=1)[0]
    if not time_str.startswith(TIME_PREFIX):
        continue
    time_str = time_str[len(TIME_PREFIX):]
    time_int = int(int(time_str) / time_interval)
    continous_cnt += 1
    non_continous_cnt = time_int - last_time_int - 1
    if last_time_int == -1:
        last_time_int = time_int
        continue
    elif last_time_int + 1 == time_int:
        last_time_int = time_int
        continue
    else:
        continous_hist_entry = continous_hist.get(continous_cnt, 0)
        continous_hist_entry += 1
        continous_hist[continous_cnt] = continous_hist_entry

        non_continous_hist_entry = non_continous_hist.get(non_continous_cnt, 0)
        non_continous_hist_entry += 1
        non_continous_hist[non_continous_cnt] = non_continous_hist_entry

        print("continous=%d, non_continous=%d, time=%s" %
              (continous_cnt, non_continous_cnt, time_str))

        continous_cnt = 0
        last_time_int = time_int
else:
    continous_hist_entry = continous_hist.get(continous_cnt, 0)
    continous_hist_entry += 1
    continous_hist[continous_cnt] = continous_hist_entry
    print("continous=%d, non_continous=%d" %
          (continous_cnt, non_continous_cnt))


continous_hist_pair = list(continous_hist.items())
continous_hist_pair.sort(key=lambda x: x[0])

non_continous_hist_pair = list(non_continous_hist.items())
non_continous_hist_pair.sort(key=lambda x: x[0])

print("continous_hist_pair", continous_hist_pair)
print("non_continous_hist_pair", non_continous_hist_pair)
