def high_and_low(numbers):
    higher = int(numbers.split()[0])
    lower = int(numbers.split()[0])
    for i in numbers.split():
        if int(i) > higher:
            higher = int(i)
        elif int(i) < lower:
            lower = int(i)
    return f"{str(higher)} {str(lower)}"
