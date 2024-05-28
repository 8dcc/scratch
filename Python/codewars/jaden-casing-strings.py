# Kata: https://www.codewars.com/kata/5390bac347d09b7da40006f6

def to_jaden_case(string):
    r = ""
    for w in string.split():
        r += w.capitalize() + " "
    return r[:-1]
