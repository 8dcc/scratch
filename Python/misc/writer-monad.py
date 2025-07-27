#!/usr/bin/env python3
#
# Python example for a Writer monad.
# See: https://8dcc.github.io/programming/understanding-monads.html

# Monadic type, expands a base integer type to add logging functionality.
class LoggedInt:
    def __init__(self, val, logs=[]):
        self.val = val
        self.logs = logs

    # Applies a one-argument monadic function to the current instance, and
    # combines the result with the existing log list.
    def bind(self, function):
        new_logged_int = function(self.val)
        return LoggedInt(
            new_logged_int.val,
            self.logs + new_logged_int.logs
        )

# ------------------------------------------------------------------------------

# Monadic functions.
def add(a, b):
    return LoggedInt(a + b, [f"Added {a} to {b}"])

def sub(a, b):
    return LoggedInt(a - b, [f"Subtracted {a} from {b}"])

def mul(a, b):
    return LoggedInt(a * b, [f"Multiplied {a} to {b}"])

def div(a, b):
    return LoggedInt(a / b, [f"Divided {a} by {b}"])

def square(a):
    return LoggedInt(a * a, [f"Squared {a}"])

# ------------------------------------------------------------------------------

def main():
    logged_result = (
        add(6, 5).bind(lambda a: sub(a, 4))
                 .bind(lambda a: mul(a, 3))
                 .bind(lambda a: div(a, 2))
                 .bind(lambda a: square(a))
    )

    print(f"Final result: {logged_result.val}")
    print("Logs:")
    for line in logged_result.logs:
        print(f"  * {line}")

if __name__ == "__main__":
    main()
