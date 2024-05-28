import re

def to_camel_case(text):
    result = ""
    first = True
    
    if ("-" in text) or ("_" in text):
        for word in re.findall(r"[a-zA-Z0-9]+", text):
            if first:
                result = result + word
                first = False
            else:
                result = result + word.lower().capitalize()
        return result
    else:
        return text
