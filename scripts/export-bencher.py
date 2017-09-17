#!/usr/bin/env python

from __future__ import print_function

import argparse
import errno
import os, os.path
import re
import shutil
import subprocess
import sys


def main(args=sys.argv):
    parser = argparse.ArgumentParser(
        prog=args[0],
        description="""""")
    parser.add_argument(
        "-v", "--verbose", dest='v', action='count', default=0,
        help="increase output verbosity (can be repeated)")
    parser.add_argument(
        "bencher_root",
        help="root directory of the bencher")
    args = parser.parse_args(args[1:])

    global ARGS
    ARGS = args
    v_print(1, "ARGS:", args)

    src_dir = os.path.join(
        os.path.dirname(os.path.realpath(__file__)), "..",
        "r")
    variants = {}
    for src in os.listdir(src_dir):
        if not src.endswith(".R"):
            continue
        stem = os.path.splitext(os.path.basename(src))[0]
        variants.setdefault(stem.partition("-")[0], []).append(src)

    for prog, srcs in variants.items():
        for idx, src in enumerate(srcs, 1):
            dst_base = ".".join([prog, "%s-%d" % ("r", idx), "r"])
            dst = os.path.join(args.bencher_root, "programs", prog, dst_base)
            try:
                os.mkdir(os.path.dirname(dst))
            except OSError as e:
                if e.errno != errno.EEXIST:
                    raise
            print(src, idx, dst)
            shutil.copy(os.path.join(src_dir, src), dst)
    return 0


def v_print(min_verbosity, *args, **kwargs):
    if ARGS.v >= min_verbosity:
        print(*args, **kwargs)


if __name__ == '__main__':
    sys.exit(main())
