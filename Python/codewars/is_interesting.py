# -----------------------------------------------------------------------
# Short version

def is_interesting(number, awesome_phrases):
    list = [number, number+1, number+2]
    if number < 100:
        return 1 if number > 97 else 0
    for i in list:
        for x in awesome_phrases:
            if str(i) == str(x):
                return 1 if i != number else 2
        if all(n == "0" for n in str(i)[1:]) or str(i).count(str(i)[0]) == len(str(i)) or str(i) in "1234567890" or str(i) in "9876543210" or str(i) == str(i)[::-1]:
            return 1 if i != number else 2
    return 0

# -----------------------------------------------------------------------
# Long version

def is_interesting(number, awesome_phrases):
    list = [number, number+1, number+2]
    
    if number < 100:
        return 1 if number > 97 else 0
    
    for i in list:
        rcode = 1 if i != number else 2
        
        for x in awesome_phrases:
            if str(i) == str(x):
                return rcode
        
        if all(n == "0" for n in str(i)[1:]):
            return rcode
        elif str(i).count(str(i)[0]) == len(str(i)):
            return rcode
        elif str(i) in "1234567890" or str(i) in "9876543210":
            return rcode
        elif str(i) == str(i)[::-1]:
            return rcode
            
    return 0
  
# -----------------------------------------------------------------------
# With wrapping

import math
def is_interesting(number, awesome_phrases):
    list = [number, number+1, number+2]
    
    if number < 100:
        return 1 if number > 97 else 0
    
    for i in list:
        rcode = 1 if i != number else 2
        
        for x in awesome_phrases:
            if str(i) == str(x):
                return rcode
        
        if all(n == "0" for n in str(i)[1:]):
            return rcode
        elif str(i).count(str(i)[0]) == len(str(i)):
            return rcode
        # https://tknk.io/RJ2O
        elif all((int(y)-int(x))%10 == 1 for x,y in zip(str(i),str(i)[1:])):
            return rcode
        elif all((int(y)-int(x))%10 == 1 for x,y in zip(str(i)[1:],str(i))):
            return rcode
        elif str(i) == str(i)[::-1]:
            return rcode
            
    return 0
  
  # -----------------------------------------------------------------------
