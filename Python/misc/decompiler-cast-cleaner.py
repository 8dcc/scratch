#!/usr/bin/env python3

import re
import sys


def strip_pointer_casts(code: str) -> str:
    """*(int32_t *)  →  *"""
    return re.sub(r'\*\(u?int(?:8|16|32|64)_t\s*\*\)', '*', code)


def strip_scalar_casts(code: str) -> str:
    """(uint64_t)expr  →  expr, only when followed by identifier, '(', or '*'."""
    return re.sub(r'\(u?int(?:8|16|32|64)_t\)\s*(?=[(\w*])', '', code)


def clean(code: str) -> str:
    code = strip_pointer_casts(code)
    code = strip_scalar_casts(code)
    return code


if __name__ == '__main__':
    sys.stdout.write(clean(sys.stdin.read()))
