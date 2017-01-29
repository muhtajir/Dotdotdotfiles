#!/usr/bin/python

import subprocess
from gi.repository.GLib import markup_escape_text

clip_cmd = ['xsel', '-bo']
clip_content = subprocess.check_output(clip_cmd).decode()
if not clip_content:
    exit()

# format output
clip_content = clip_content.splitlines()[0]
if len(clip_content) > 20:
    clip_content = clip_content[0:20] + '…'

clip_content = markup_escape_text(clip_content)
clip_content = ' ' + '<span font_size="small">' + clip_content + '</span>'

print(clip_content)
