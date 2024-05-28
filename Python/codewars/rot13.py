def rot13(message):
    x = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    y = "nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM"
    result = ""
    for i in message:
        if i in x:
            result = result + (y[x.index(i)])
        else:
            result = result + i
    return result
    
    # Only works with lowercase
    #return ''.join((chr(97+(ord(letter)-97+13) % 26) for letter in message))
