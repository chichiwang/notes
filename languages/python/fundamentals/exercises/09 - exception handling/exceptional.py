'''A module for demonstrating exceptions

To run it cd into this directory and start a REPL.
Input:
>>> import exceptional
>>> exceptional.convert_0('33')
33
>>> exceptional.convert_0('foobar')
-1
>>> exceptional.convert_1([0, 1, 2])
-1
'''

# Capture a single error type
def convert_0(s):
    '''Convert to an integer'''
    try:
        x = int(s)
    except ValueError:
        x = -1
    return x

# Capture multiple error types
def convert_1(s):
    '''Convert to an integer'''
    try:
        x = int(s)
    # Apply `except` keyword to a tuple of exception types
    except (ValueError, TypeError):
        x = -1
    return x
