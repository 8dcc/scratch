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

    input_str = sys.argv[1].strip()
    dict_path = sys.argv[2]

    # Make sure the dictionary exists
    if not os.path.isfile(dict_path):
        print("The path '%s' is not a file" % dict_path)
        exit(1)

    # Iterate each line of the dictionary
    with open(dict_path) as f:
        words = [line.strip() for line in f]

    # Initialize the indexes array. For more information, see:
    # https://github.com/8dcc/scratch/blob/main/C/algorithms/variable-nested-loops.c
    input_word_count = len(input_str.split())
    indexes = []
    for i in range(input_word_count):
        indexes.append(0)

    while indexes[0] < len(words):
        # Join all words in the iteration into a string
        total_string = ""
        for i in range(input_word_count):
            total_string += words[indexes[i]] + " "
        total_string = total_string.rstrip()

        # Check if the word combination is a possible pseudonym
        if valid_pseudonym(input_str.replace(" ", "").lower(),
                           total_string.replace(" ", "").lower()):
            print(total_string)

        for cur_idx in reversed(range(input_word_count)):
            if cur_idx == 0 or indexes[cur_idx] + 1 < len(words):
                indexes[cur_idx] += 1
                break

            indexes[cur_idx] = 0

if __name__ == '__main__':
    main()
