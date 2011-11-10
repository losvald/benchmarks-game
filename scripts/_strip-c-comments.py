#!/usr/bin/python
import re, sys

if __name__ == '__main__':
    if len(sys.argv) < 1:
        print >> sys.stderr, "Usage: %s file ..." % sys.argv[0]
        sys.exit(1)

    def replacer(match):
        s = match.group(0)
        if s.startswith('/'):
            return ""
        else:
            return s
    pattern = re.compile(
        r'//.*?$|/\*.*?\*/|\'(?:\\.|[^\\\'])*\'|"(?:\\.|[^\\"])*"',
        re.DOTALL | re.MULTILINE)

    for f in sys.argv[1:]:
        text = ''.join(open(f).readlines())
        print re.sub(pattern, replacer, text)
