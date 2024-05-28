import math
def comp(array1, array2):
    if array1 == None or array2 == None or len(array1) != len(array2):
        return False
    array1 = [abs(n) for n in array1]
    for i in array2:
        if math.sqrt(i) not in array1:
            return False
        else:
            array1.remove(math.sqrt(i))
    return True
