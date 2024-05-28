import itertools
def get_pins(observed):
    result = []
    list1 = []
    posibilities = {
        "1" : ["1", "2", "4"],
        "2" : ["2", "1", "3", "5"],
        "3" : ["3", "2", "6"],
        "4" : ["4", "1", "5", "7"],
        "5" : ["5", "2", "4", "6", "8"],
        "6" : ["6", "3", "5", "9"],
        "7" : ["7", "4", "8"],
        "8" : ["8", "7", "5", "9", "0"],
        "9" : ["9", "8", "6"],
        "0" : ["0", "8"]
    }
    
    for n in observed:
        list1.append(posibilities[n])
    list2 = list(itertools.product(*list1))
    for i in list2:
        result.append("".join(i))
    return result
