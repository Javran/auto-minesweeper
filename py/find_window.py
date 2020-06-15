#!/usr/bin/env python3.7
"""Finds a window that has minesweeper opened.
"""

import json
import re
import subprocess
import sys

import cv2
import numpy


RE_XWININFO_WIN_ID_EXTRACT = re.compile(r'^xwininfo: Window id: (\d+) ',
                                        re.MULTILINE)
PAT_HARD = cv2.imread('assets/hard.jpg',cv2.IMREAD_COLOR)


def canny(img):
    return cv2.Canny(img,0,1)


def find_window_id():
    """Finds window id."""
    print('Left click to select a window that has minesweeper running ...',
          file=sys.stderr)
    result = subprocess.run(['xwininfo', '-int'],
                            stdout=subprocess.PIPE, check=True, encoding='utf8')
    return int(RE_XWININFO_WIN_ID_EXTRACT.search(result.stdout).group(1))


def recognize_minesweeper(window_id, img_filter=lambda x: x):
    """Recognizes minesweeper game and return position of top-left corner."""
    result = subprocess.run(['import',
                             '-window', str(window_id),
                             'png:-'],
                            stdout=subprocess.PIPE, check=True)
    nparr = numpy.frombuffer(result.stdout, numpy.uint8)
    screenshot = cv2.imdecode(nparr, cv2.IMREAD_COLOR)
    img, pat = img_filter(screenshot), img_filter(PAT_HARD)
    res = cv2.matchTemplate(img, pat, cv2.TM_CCORR_NORMED)
    _, max_val, _, max_loc = cv2.minMaxLoc(res)
    assert max_val >= 0.994, 'Cannot find minesweeper in the selected window.'
    pat_x, pat_y = max_loc
    # TODO: make this more customization by attaching some metadata
    # to the pattern asset.
    return (pat_x - 12, pat_y + 49)


if __name__ == '__main__':
    window_id = find_window_id()
    top_left = recognize_minesweeper(window_id)
    print(json.dumps(
        {
            'window_id': window_id,
            'top_left': top_left,
            # columns and rows
            'tiles_shape': (24,20),
            # size of each tile
            'tile_size': (25,25),
            # shrink tile size (for template matching)
            'tile_shrink': 3,
        },
        separators=(',', ':')))

