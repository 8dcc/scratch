def namelist(names):
    counter = 1
    result = ''
    for i in names:
        if counter == 1:
            result = i['name']
        elif counter == len(names):
            result = f"{result} & {i['name']}"
        else:
            result = f"{result}, {i['name']}"
        counter += 1
    return result 
