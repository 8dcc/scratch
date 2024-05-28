# Kata: https://www.codewars.com/kata/52f677797c461daaf7000740

# ------------------------------------------------------------------------------------

# Always timeout...:(
def solution(a):
    while a[0] != a[-1]:
        a.sort(reverse = True)  # Sort to make it easier
        for idx in range(len(a[:-1])):
            if a[0] > n2:
                a[idx] = n1 - n2

    r = a[0] * len(a)
    return r

# Even faster (~93/100)
def solution(a):
    while a[0] != a[-1]:
        a.sort(reverse = True)  # Sort to make it easier
        for idx in range(len(a[:-1])):
            n1 = a[idx]
            n2 = a[idx+1]
            if n1 > n2:
                a[idx] = n1 - n2

    r = a[0] * len(a)
    return r

# Very close but timeout
def solution(a):
    while a[0] != a[-1]:
        a.sort(reverse = True)  # Sort to make it easier
        for idx1, n1 in enumerate(a[:-1]):
            n2 = a[idx1+1]
            if n1 > n2:
                a[idx1] = n1 - n2

    r = a[0] * len(a)
    return r

# Timeout as well
def solution(a):
    a.sort()            # Sort to make it easier
    converted = True    # Used to know if you reached the smallest possible state
    while converted:
        converted = False
        for idx1, n1 in enumerate(a):
            if idx1 > 0:
                n2 = a[idx1-1]
                if n1 > n2:
                    a[idx1] = n1 - n2
                    converted = True
                elif n2 > n1:
                    a[idx1-1] = n2 - n1
                    converted = True

    r = 0
    for final in a:
        r += final
    return r

# Good, but too many iterations, so timeout
def solution(a):
    converted = True   # Used to know if you reached the smallest possible state
    while converted:        # 2 many iterations frfr
        converted = False
        for id1, n1 in enumerate(a):
            for id2, n2 in enumerate(a):
                if n1 > n2:
                    a[id1] = n1 - n2
                    converted = True
                elif n2 > n1:
                    a[id2] = n2 - n1
                    converted = True

    r = 0
    for final in a:
        r += final
    return r
