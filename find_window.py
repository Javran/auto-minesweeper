#!/usr/bin/env python3
"""Finds a window that has minesweeper opened.
"""

import re
import subprocess


RE_XWININFO_WIN_ID_EXTRACT = re.compile(r'^xwininfo: Window id: (\d+) ', re.MULTILINE)


def find_window_id():
    print('Left click to select a window that has minesweeper running ...')
    result = subprocess.run(['xwininfo', '-int'],
                            stdout=subprocess.PIPE, check=True, encoding='utf8')
    return int(RE_XWININFO_WIN_ID_EXTRACT.search(result.stdout).group(1))


if __name__ == '__main__':
    print(find_window_id())
