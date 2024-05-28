import math

def find_next_square(sq):
    if str(math.sqrt(sq)).split(".")[1] is "0":
        var = int(str(math.sqrt(sq)).split(".")[0]) + 1
        return math.pow(var, 2)
    else:
        return -1
