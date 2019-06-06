"""
Prints a_var in both instances since a_var is defined in the global scope. Both accesses of a_var can reach it.
"""
a_var = 'global variable'

def a_func():
    print(a_var, '[ a_var inside a_func() ]')

a_func()
print(a_var, '[ a_var outside a_func() ]')
