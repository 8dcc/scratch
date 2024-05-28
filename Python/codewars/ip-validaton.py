# Kata: https://www.codewars.com/kata/515decfd9dcfc23bb6000006

import re

def is_valid_IP(s):
    if s.strip() != s:
        return False

    if re.search("^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$", s):
        for n in s.split("."):
            if len(n) > 1 and n[0] == '0':
                return False
            if int(n) not in range(0,256):
                return False
    else:
        return False

    return True
