def nb_year(p0, percent, aug, p):
    result = 0
    while p0 < p:
        result += 1
        p0 = p0 + int(p0 * (percent/100)) + aug
    return result
