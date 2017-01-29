#!/usr/bin/python

import re

todo_loc = '/home/nicolai/.local/share/todo.txt/todo.txt'

with open(todo_loc) as todo_file:
    lines = todo_file.read()
    urgent = re.findall(r'^\(A\)\s', lines, flags=re.M)
    urgent_len = str(len(urgent))
    tasks = str(len(lines.splitlines()))
    urgent_tasks = '<span font_size ="small"> (<span foreground="#dd464c">' \
            + urgent_len + '</span>)</span> '
    output = ' ' + tasks + urgent_tasks + 'Aufgaben'
    print(output)
