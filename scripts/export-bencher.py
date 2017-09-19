#!/usr/bin/env python

from __future__ import print_function

import argparse
import errno
import glob
from itertools import chain
import os.path
import re
import shutil
import sys


def main(args=sys.argv):
    parser = argparse.ArgumentParser(
        prog=args[0],
        description="""""")
    parser.add_argument(
        "-v", "--verbose", dest='v', action='count', default=0,
        help="increase output verbosity (can be repeated)")
    parser.add_argument(
        "-n", "--dry-run", action='store_true',
        help="do not make side effects, just print what would be done")
    parser.add_argument(
        "bencher_root",
        help="root directory of the bencher")
    args = parser.parse_args(args[1:])

    global ARGS
    ARGS = args

    root_dir = os.path.join(os.path.dirname(os.path.realpath(__file__)), "..")
    ext_map = {"R": "r", "py": "python"}
    name_map = {
        'reversecomplement': 'revcomp',
    }
    lang_variants = {}
    for src in chain(
        glob.glob(root_dir + "/r/*"),
        glob.iglob(root_dir + "/other-lang/**/*.*", recursive=True)
    ):
        stem, ext = os.path.splitext(os.path.basename(src))
        ext = ext[1:]
        if ext not in ext_map:
            v_print(2, "skip:", src)
            continue
        lang_variants.setdefault(ext_map[ext], {}).setdefault(
            stem.partition("-")[0], []).append(src)

    for lang, variants in lang_variants.items():
        for prog, srcs in variants.items():  # be compatible with Python 3
            prog = name_map.get(prog, prog)
            for idx, src_path in enumerate(srcs, 1):
                dst = ".".join([prog, "%s-%d" % (lang, idx), lang])
                dst_path = os.path.join(args.bencher_root, "programs",
                                        prog, dst)
                try:
                    os.mkdir(os.path.dirname(dst_path))
                except OSError as e:
                    if e.errno != errno.EEXIST:
                        raise
                if ARGS.dry_run:
                    print("cp", src_path, dst_path)
                else:
                    v_print(1, os.path.basename(src_path), "->", dst)
                    shutil.copy(src_path, dst_path)
    return 0


def v_print(min_verbosity, *args, **kwargs):
    kwargs.setdefault('file', sys.stderr)
    if ARGS.v >= min_verbosity:
        print(*args, **kwargs)


if __name__ == '__main__':
    sys.exit(main())
