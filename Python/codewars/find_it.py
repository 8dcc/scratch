def find_it(seq):
    for n in set(seq):
        if seq.count(n) % 2 != 0:
            return n
