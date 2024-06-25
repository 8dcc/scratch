#!/usr/bin/python3

import sys, os, string

def valid_pseudonym(str_a, str_b):
    if (len(str_a) != len(str_b)):
        return False

    for c in set(str_a):
        if str_a.count(c) != str_b.count(c):
            return False

    return True

def main():
    if (len(sys.argv) < 3):
        print("Usage: %s <str> <dict-file>" % sys.argv[0])
        exit(1)

    input_str = sys.argv[1].strip().replace(" ", "")
    dict_path = sys.argv[2]

    # Make sure the dictionary exists
    if not os.path.isfile(dict_path):
        print("The path '%s' is not a file" % dict_path)
        exit(1)

    # Iterate each line of the dictionary
    with open(dict_path) as f:
        for line in f:
            line = line.strip().lower()
            if valid_pseudonym(input_str.lower(), line):
                print(line)

if __name__ == '__main__':
    main()
