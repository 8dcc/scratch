def longest(a1, a2):
    result = []
    full = a1+a2
    for n in full:
        if n not in result:
            result.append(n)
    return "".join(sorted(result))
