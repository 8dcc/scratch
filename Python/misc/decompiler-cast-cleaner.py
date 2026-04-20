#!/usr/bin/env python3

import re
import sys

# ---------------------------------------------------------------------------
# Cast stripping
# ---------------------------------------------------------------------------

def strip_pointer_casts(code: str) -> str:
    """
    Convert:
        *(int32_t *)  ->  *
    """
    return re.sub(r'\*\(u?int(?:8|16|32|64)_t\s*\*\)', '*', code)


def strip_scalar_casts(code: str) -> str:
    """
    Convert:
        (uint64_t)expr  ->  expr
    Only when followed by identifier, '(', or '*'.
    """
    return re.sub(r'\(u?int(?:8|16|32|64)_t\)\s*(?=[(\w*])', '', code)

# ---------------------------------------------------------------------------
# Method call simplification
# ---------------------------------------------------------------------------

# Lowercase tokens that signal the start of a type suffix in a mangled name.
KNOWN_TYPES = {
    'int', 'uint', 'unsigned', 'signed', 'long', 'short', 'char',
    'float', 'double', 'void', 'bool', 'byte', 'size', 'const',
    'int8', 'int16', 'int32', 'int64', 'uint8', 'uint16', 'uint32', 'uint64',
}

def trim_method_name(name: str) -> str:
    """
    Given a raw mangled method name, return the human-readable portion.

    Rules (applied in order):
      1. Strip a leading underscore (ctor/dtor artifact: _QRegExp → QRegExp).
      2. Treat '__' as a hard stop — discard everything from it onward.
      3. Walk '_'-separated tokens left to right; stop before the first token
         that is a known C type keyword or starts with an uppercase letter.

    Examples:
      reallocData_unsigned_int__QFlags  →  reallocData
      toLatin1_helper_inplace_QString   →  toLatin1_helper_inplace
      operator__QString_const           →  operator
      cap_int                           →  cap
      _QRegExp                          →  QRegExp
    """
    name = name.lstrip('_')

    # Hard stop at first '__'
    double_under = name.find('__')
    ceiling = name[:double_under] if double_under != -1 else name

    tokens = ceiling.split('_')
    kept = []
    for tok in tokens:
        if not kept:
            kept.append(tok)
            continue
        if tok in KNOWN_TYPES or (tok and tok[0].isupper()):
            break
        kept.append(tok)

    return '_'.join(kept)


def replace_method(m: re.Match) -> str:
    class_name = m.group(1)
    raw_method = m.group(2)
    has_call   = (m.group(3) == '(')
    method_name = trim_method_name(raw_method)

    result = f'{class_name}.{method_name}'
    return result + ('(' if has_call else result)


def simplify_methods(code: str) -> str:
    """
    method.ClassName.rawMethod(args)  →  ClassName.method(args)
    method.ClassName.rawMethod        →  ClassName.method
    """
    pattern = re.compile(
        r'\bmethod\.(\w+)\.([\w:]+)'  # method.ClassName.rawMethod (including :: namespaces)
        r'(\(|(?=\W))'                # '(' for a call, or lookahead for non-word (value ref)
    )
    return pattern.sub(replace_method, code)


# ---------------------------------------------------------------------------
# Pipeline
# ---------------------------------------------------------------------------

def clean(code: str) -> str:
    code = strip_pointer_casts(code)
    code = strip_scalar_casts(code)
    code = simplify_methods(code)
    return code


if __name__ == '__main__':
    sys.stdout.write(clean(sys.stdin.read()))
