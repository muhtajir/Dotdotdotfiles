# pylint: disable=C0111
import os
import tempfile
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
config = config  # type: ConfigAPI # noqa: F821 pylint: disable=E0602,C0103
c = c  # type: ConfigContainer # noqa: F821 pylint: disable=E0602,C0103


def merge_bookmarks():
    """Merge bookmarks from the cloud with those kept by qutebrowser."""
    int_bookmarks = os.path.join(config.configdir, 'bookmarks', 'urls')
    ext_bookmarks = os.path.join(os.getenv('HOME'), 'Nextcloud', 'Diverses',
                                 'Bookmarks', 'qute_urls')

    try:
        with open(ext_bookmarks) as f:
            ext_urls = f.readlines()
    except FileNotFoundError:
        return

    f_mode = 'r+' if os.path.exists(int_bookmarks) else 'w+'
    with open(int_bookmarks, f_mode) as f:
        int_urls = f.readlines()
        f.seek(0)
        f.truncate()
        f.writelines(sorted(set(ext_urls + int_urls)))


BASE00 = '#{}'.format(os.getenv('__BASE00'))  # black0
BASE01 = '#{}'.format(os.getenv('__BASE01'))  # black1
BASE02 = '#{}'.format(os.getenv('__BASE02'))  # black2
BASE03 = '#{}'.format(os.getenv('__BASE03'))  # black3
BASE04 = '#{}'.format(os.getenv('__BASE04'))  # black4
BASE05 = '#{}'.format(os.getenv('__BASE05'))  # white0
BASE06 = '#{}'.format(os.getenv('__BASE06'))  # white1
BASE07 = '#{}'.format(os.getenv('__BASE07'))  # white2
BASE08 = '#{}'.format(os.getenv('__BASE08'))  # red
BASE09 = '#{}'.format(os.getenv('__BASE09'))  # light_orange
BASE0A = '#{}'.format(os.getenv('__BASE0A'))  # yellow
BASE0B = '#{}'.format(os.getenv('__BASE0B'))  # green
BASE0C = '#{}'.format(os.getenv('__BASE0C'))  # cyan
BASE0D = '#{}'.format(os.getenv('__BASE0D'))  # blue
BASE0E = '#{}'.format(os.getenv('__BASE0E'))  # magenta
BASE0F = '#{}'.format(os.getenv('__BASE0F'))  # dark_orange

FONT_SANS = os.getenv('FONT_SANS')
FONT_MONO = os.getenv('FONT_MONO')

merge_bookmarks()

## Color settings
# Background color of the completion widget category headers.
c.colors.completion.category.bg = BASE00
# Bottom border color of the completion widget category headers.
c.colors.completion.category.border.bottom = BASE02
# Top border color of the completion widget category headers.
c.colors.completion.category.border.top = BASE00
# Foreground color of completion widget category headers.
c.colors.completion.category.fg = BASE07
# Background color of the completion widget for odd rows.
c.colors.completion.odd.bg = BASE01
# Background color of the completion widget for even rows.
c.colors.completion.even.bg = BASE02
# Text color of the completion widget.
c.colors.completion.fg = BASE07
# Background color of the selected completion item.
c.colors.completion.item.selected.bg = BASE07
# Bottom border color of the selected completion item.
c.colors.completion.item.selected.border.bottom = BASE07
# Top border color of the completion widget category headers.
c.colors.completion.item.selected.border.top = BASE07
# Foreground color of the selected completion item.
c.colors.completion.item.selected.fg = BASE00
# Foreground color of the matched text in the completion.
c.colors.completion.match.fg = BASE08
# Color of the scrollbar in completion view
c.colors.completion.scrollbar.bg = BASE01
# Color of the scrollbar handle in completion view.
c.colors.completion.scrollbar.fg = BASE07
# Background color for the download bar.
c.colors.downloads.bar.bg = BASE00
# Background color for downloads with errors.
c.colors.downloads.error.bg = BASE08
# Foreground color for downloads with errors.
c.colors.downloads.error.fg = BASE07
# Color gradient start for download backgrounds.
c.colors.downloads.start.bg = BASE0D
# Color gradient start for download text.
c.colors.downloads.start.fg = BASE00
# Color gradient stop for download backgrounds.
c.colors.downloads.stop.bg = BASE0B
# Color gradient end for download text.
c.colors.downloads.stop.fg = BASE00
# Color gradient interpolation system for download backgrounds.
# Valid values:
c.colors.downloads.system.bg = 'rgb'
# Color gradient interpolation system for download text.
# Valid values:
c.colors.downloads.system.fg = 'rgb'
# Background color for hints. Note that you can use a `rgba(...)` value
# for transparency.
# for base16: rgba(light_orange), rgba(dark_orange)
c.colors.hints.bg = 'rgba(255, 247, 133, 0.9)'
# Font color for hints.
c.colors.hints.fg = BASE00
# Font color for the matched part of hints.
c.colors.hints.match.fg = BASE0B
# Background color of the keyhint widget.
# for base16: rgba(black0)
c.colors.keyhint.bg = 'rgba(38, 38, 38, 0.9)'
# Text color for the keyhint widget.
c.colors.keyhint.fg = BASE07
# Highlight color for keys to complete the current keychain.
c.colors.keyhint.suffix.fg = BASE0A
# Background color of an error message.
c.colors.messages.error.bg = BASE08
# Border color of an error message.
c.colors.messages.error.border = BASE08
# Foreground color of an error message.
c.colors.messages.error.fg = BASE07
# Background color of an info message.
c.colors.messages.info.bg = BASE00
# Border color of an info message.
c.colors.messages.info.border = BASE01
# Foreground color an info message.
c.colors.messages.info.fg = BASE07
# Background color of a warning message.
c.colors.messages.warning.bg = BASE0F
# Border color of a warning message.
c.colors.messages.warning.border = BASE0F
# Foreground color a warning message.
c.colors.messages.warning.fg = BASE07
# Background color for prompts.
c.colors.prompts.bg = BASE02
# Border used around UI elements in prompts.
c.colors.prompts.border = '1px {}'.format(BASE07)
# Foreground color for prompts.
c.colors.prompts.fg = BASE07
# Background color for the selected item in filename prompts.
c.colors.prompts.selected.bg = BASE03
# Background color of the statusbar in caret mode.
c.colors.statusbar.caret.bg = BASE0F
# Foreground color of the statusbar in caret mode.
c.colors.statusbar.caret.fg = BASE07
# Background color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.bg = BASE0E
# Foreground color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.fg = BASE07
# Background color of the statusbar in command mode.
c.colors.statusbar.command.bg = BASE00
# Foreground color of the statusbar in command mode.
c.colors.statusbar.command.fg = BASE07
# Background color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.bg = BASE02
# Foreground color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.fg = BASE07
# Background color of the statusbar in private browsing mode.
c.colors.statusbar.private.bg = BASE02
# Foreground color of the statusbar in private browsing mode.
c.colors.statusbar.private.fg = BASE07
# Background color of the statusbar in insert mode.
c.colors.statusbar.insert.bg = BASE0F
# Foreground color of the statusbar in insert mode.
c.colors.statusbar.insert.fg = BASE07
# Background color of the statusbar.
c.colors.statusbar.normal.bg = BASE00
# Foreground color of the statusbar.
c.colors.statusbar.normal.fg = BASE07
# Background color of the progress bar.
c.colors.statusbar.progress.bg = BASE07
# Foreground color of the URL in the statusbar on error.
c.colors.statusbar.url.error.fg = BASE09
# Default foreground color of the URL in the statusbar.
c.colors.statusbar.url.fg = BASE07
# Foreground color of the URL in the statusbar for hovered links.
c.colors.statusbar.url.hover.fg = BASE0D
# Foreground color of the URL in the statusbar on successful load
# (http).
c.colors.statusbar.url.success.http.fg = BASE07
# Foreground color of the URL in the statusbar on successful load
# (https).
c.colors.statusbar.url.success.https.fg = BASE0C
# Foreground color of the URL in the statusbar when there's a warning.
c.colors.statusbar.url.warn.fg = BASE0A
# Background color of the tab bar.
c.colors.tabs.bar.bg = BASE03
# Background color of unselected even tabs.
c.colors.tabs.even.bg = BASE02
# Foreground color of unselected even tabs.
c.colors.tabs.even.fg = BASE07
# Color for the tab indicator on errors.
c.colors.tabs.indicator.error = BASE08
# Color gradient start for the tab indicator.
c.colors.tabs.indicator.start = BASE0D
# Color gradient end for the tab indicator.
c.colors.tabs.indicator.stop = BASE0B
# Color gradient interpolation system for the tab indicator.
# Valid values:
#   - rgb: Interpolate in the RGB color system.
#   - hsv: Interpolate in the HSV color system.
#   - hsl: Interpolate in the HSL color system.
#   - none: Don't show a gradient.
c.colors.tabs.indicator.system = 'rgb'
# Background color of unselected odd tabs.
c.colors.tabs.odd.bg = BASE01
# Foreground color of unselected odd tabs.
c.colors.tabs.odd.fg = BASE07
# Background color of selected even tabs.
c.colors.tabs.selected.even.bg = BASE07
# Foreground color of selected even tabs.
c.colors.tabs.selected.even.fg = BASE00
# Background color of selected odd tabs.
c.colors.tabs.selected.odd.bg = BASE07
# Foreground color of selected odd tabs.
c.colors.tabs.selected.odd.fg = BASE00
# Background color for webpages if unset (or empty to use the theme's
# color)
c.colors.webpage.bg = 'white'
# Background color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.bg = BASE07
# Foreground color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.fg = BASE00


## Font settings
# Only let completion use up as little space as possible
c.completion.shrink = True
# Font used in the completion categories.
c.fonts.completion.category = 'bold 9pt {}'.format(FONT_MONO)
# Font used in the completion widget.
c.fonts.completion.entry = '9pt {}'.format(FONT_MONO)
# Font used for the debugging console.
c.fonts.debug_console = '9pt {}'.format(FONT_MONO)
# Font used for the downloadbar.
c.fonts.downloads = '9pt {}'.format(FONT_SANS)
# Font used for the hints.
c.fonts.hints = 'bold 10pt {}'.format(FONT_SANS)
# Font used in the keyhint widget.
c.fonts.keyhint = '9pt {}'.format(FONT_SANS)
# Font used for error messages.
c.fonts.messages.error = '9pt {}'.format(FONT_SANS)
# Font used for info messages.
c.fonts.messages.info = '9pt {}'.format(FONT_SANS)
# Font used for warning messages.
c.fonts.messages.warning = '9pt {}'.format(FONT_SANS)
# Font used for prompts.
c.fonts.prompts = '9pt {}'.format(FONT_SANS)
# Font used in the statusbar.
c.fonts.statusbar = '9pt {}'.format(FONT_MONO)
# Font used in the tab bar.
c.fonts.tabs = '10pt {}'.format(FONT_SANS)
# Nice looking padding for tabs headers
c.tabs.padding = {'top': 2, 'bottom': 2, 'left': 3, 'right': 3}

## Hint settings
# Mode to use for hints.
# very silly way to decide which mode i want to use
c.hints.mode = 'number'
# A comma-separated list of regexes to use for 'next' links.
c.hints.next_regexes = ['\\bnext\\b', '\\bmore\\b', '\\bnewer\\b',
                        '\\b[>→≫]\\b', '\\b(>>|»)\\b', '\\bcontinue\\b']
# A comma-separated list of regexes to use for 'prev' links.
c.hints.prev_regexes = ['\\bprev(ious)?\\b', '\\bback\\b', '\\bolder\\b',
                        '\\b[<←≪]\\b', '\\b(<<|«)\\b']
# Time from pressing a key to seeing the keyhint dialog (ms).
c.keyhint.delay = 200
# A timeout (in milliseconds) to ignore normal-mode key bindings after a
# successful auto-follow.
c.hints.auto_follow_timeout = 300

## Address bar shortcut settings
# Definitions of search engines which can be used via the address bar.
c.url.searchengines = {
    'DEFAULT': 'https://duckduckgo.com/?q={}',
    'aw': 'https://wiki.archlinux.org/index.php?search={}',
    'bc': 'https://bandcamp.com/search?q={}',
    'bgg': 'https://boardgamegeek.com/geeksearch.php?action=search&objecttype=boardgame&q={}',
    'fm': 'https://www.last.fm/search?q={}',
    'g': 'https://encrypted.google.com/search?q={}',
    'gh': 'https://github.com/search?utf8=%E2%9C%93&q={}',
    'lc': 'https://dict.leo.org/chinesisch-deutsch/{}',
    'le': 'https://dict.leo.org/german-english/{}',
    'lf': 'https://dict.leo.org/französisch-deutsch/{}',
    'mel': 'http://melpa.org/#/?q={}',
    'py': 'https://docs.python.org/3.6/search.html?q={}',
    'ug': 'https://www.ultimate-guitar.com/search.php?search_type=title&value={}',
    'w': 'https://en.wikipedia.org/w/index.php?search={}',
    'wd': 'https://de.wikipedia.org/w/index.php?search={}',
    'yt': 'https://www.youtube.com/results?search_query={}'}
# Aliases
c.aliases['ssh-tunnel'] = 'set content.proxy socks://localhost:4711 ;; message-info "Tunnel proxy set."'

## Miscellaneous settings
# Start page
c.url.start_pages = 'https://www.punknews.org/'
# Where to show the downloaded files.
c.downloads.position = 'bottom'
# Default download location
c.downloads.location.directory = tempfile.gettempdir()
# Open new tabs in background
c.tabs.background = True
# Don't store cookies because I don't like them
c.content.cookies.store = False
# Set editor
c.editor.command = ['termite', '-e',
                    'nvim -c "normal {line}G{column0}l" {file}']
# Which tab to select when the focused tab is removed.
c.tabs.select_on_remove = 'last-used'
# Confirm exit when there's downloads running
c.confirm_quit = ['downloads']

## Key bindings
# Deletion
config.unbind('d', mode='normal')
config.unbind('D', mode='normal')
config.bind('dd', 'tab-close')
config.bind('dD', 'tab-only')
config.bind('dl', 'download-cancel')
config.bind('dq', 'set-cmd-text --space :quickmark-del')
config.bind('db', 'set-cmd-text --space :bookmark-del')
# Clear search highlightingg
config.bind('<Ctrl-´>', 'search')
# Quickmark/Bookmark opening (mostly redundant, replace e bindings if
# necessary)
config.bind('eq', 'set-cmd-text --space :quickmark-load')
config.bind('eb', 'set-cmd-text --space :bookmark-load')
# Completion navigation command mode
config.bind('<Alt-k>', 'command-history-prev', mode='command')
config.bind('<Alt-j>', 'command-history-next', mode='command')
config.bind('<Ctrl-n>', 'completion-item-focus next', mode='command')
config.bind('<Ctrl-p>', 'completion-item-focus prev', mode='command')
# Additional hinting
config.bind(',f', 'hint all hover')
config.bind(',O', 'set-cmd-text --space :open -b')
config.bind(';v', 'hint links spawn --detach mpv {hint-url}')
config.bind(';a', 'hint links spawn --detach mpv --no-video --player-operation-mode=pseudo-gui {hint-url}')
config.bind(';x', 'hint links userscript xdg-open')
# Open current url in new windows
config.unbind('wO', mode='normal')
config.bind('gw', 'set-cmd-text :open -w {url:pretty}')
# Buffer navigation
config.bind('b', 'set-cmd-text --space :buffer')
# Source config
config.bind('<Ctrl-R>', 'config-source')
# Passthrough settings
config.unbind('<Ctrl-v>', mode='normal')
config.unbind('<Ctrl-v>', mode='passthrough')
config.bind('<Ctrl-\>', 'enter-mode passthrough')
config.bind('<Ctrl-\>', 'leave-mode', mode='passthrough')
config.bind('<Shift-Escape>', 'fake-key <Escape>')
