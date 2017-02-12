#!/usr/bin/python

# This script displays the contents of the clipboard, cut down to the first 20
# characters. If the clipboard is empty, nothing is shown. Whenever the
# blocklet is clicked (with any mouse button), the clipboard is emptied and the
# blocklet disappears.

import re
import subprocess
from gi.repository.GLib import markup_escape_text
from os import environ

# clicking the blocklet clears the clipboard
if not environ['BLOCK_BUTTON'] == '':
    subprocess.call(['xsel', '-bc'])
    print('')
    exit()

clip_cmd = ['xsel', '-bo']
clip_content = subprocess.check_output(clip_cmd).decode()
if not clip_content:
    exit()

# format output
clip_content = clip_content.splitlines()[0]
# no tabs. json doesn't like tabs.
clip_content = re.sub(r'(\t)', '\x20', clip_content)

if len(clip_content) > 20:
    clip_content = clip_content[0:20] + '…'

clip_content = markup_escape_text(clip_content)
clip_content = '<span font_size="small">' + clip_content + '</span>'

print(clip_content)
