def duplicate_encode(word):
    result = ""
    for i in word.lower():
        if word.lower().count(i) > 1:
            result = result + ")"
        else:
            result = result + "("
    return result
    
# -------------------------------------------------------
    
def duplicate_encode(word):
    result = ""
    for i in word.lower():
        result = result + ")" if word.lower().count(i) > 1 else result + "("
    return result
