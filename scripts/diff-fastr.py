#!/usr/bin/env python

from __future__ import print_function

import argparse
from glob import iglob
import os.path
import subprocess
import sys


def _stem(path):
    return os.path.splitext(os.path.basename(path))[0]


def normalize_r_files(root):
    for path in iglob(os.path.join(root, "r", "*.R")):
        stem = _stem(path)
        if stem not in 'run-dynamic':
            yield stem, path


def normalize_fastr_files(root):
    for path in iglob(os.path.join(root, "test", "r", "shootout", "*", "*.r")):
        yield _stem(path), path


def main(args=sys.argv):
    script_dir = os.path.dirname(os.path.realpath(__file__))
    parser = argparse.ArgumentParser(
        prog=args[0],
        description="""""")
    parser.add_argument(
        "-v", "--verbose", dest='v', action='count', default=0,
        help="increase output verbosity (can be repeated)")
    parser.add_argument(
        "--no-diff", action='store_true',
        help="do not show diff output, just the common files")
    parser.add_argument(
        "fastr_root",
        help="root directory of the fastr repository")
    args = parser.parse_args(args[1:])
    global ARGS
    ARGS = args
    v_print(1, "ARGS:", args)

    this_root = os.path.dirname(script_dir)
    these_files = dict(normalize_r_files(this_root))
    fastr_files = dict(normalize_fastr_files(args.fastr_root))
    if not fastr_files:
        parser.error("invalid root for fastr")

    common_names = set(these_files) & set(fastr_files)

    def report_missing_paths(expected_files, actual_files):
        for missing in set(expected_files) - set(actual_files):
            print(missing, "@", expected_files[missing])

    print("Only here")
    report_missing_paths(fastr_files, these_files)
    print("Only in fastr")
    report_missing_paths(these_files, fastr_files)

    diff_count = 0
    if args.no_diff:
        print("Common")
        for name in common_names:
            print("%s: here@%s fastr@%s\n" % (
                name,
                os.path.relpath(these_files[name], this_root),
                os.path.relpath(fastr_files[name], args.fastr_root),
            ))
    else:
        for name in common_names:
            print("\n@", name)
            ret_code = subprocess.call([
                    "diff", these_files[name], fastr_files[name]]
            )
            if ret_code == 2:
                print("error when trying to diff " + name, file=sys.stderr)
                return 1
            diff_count += bool(ret_code)

    return diff_count


def v_print(min_verbosity, *args, **kwargs):
    if ARGS.v >= min_verbosity:
        print(*args, **kwargs)


if __name__ == '__main__':
    sys.exit(main())
