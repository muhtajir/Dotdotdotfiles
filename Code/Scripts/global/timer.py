#!/bin/env python

import sys
import time
timer_time = 0

while True:
    mins = timer_time / 60
    secs = timer_time % 60
    output = '{:02.0f}:{:02}'.format(mins, secs)
    print(output, sep=' ', end='\r', flush=True)
    sys.stdout.flush()
    time.sleep(1)
    timer_time += 1
