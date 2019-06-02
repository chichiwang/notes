"""
Output:
local value [ a_var inside a_func() ]
global value [ a_var outside a_func() ]

Rationale:
a_var is defined in the global scope and also defined inside of function scope. Each access reads the closest scope of the variable.
"""
a_var = 'global value'

def a_func():
    a_var = 'local value'
    print(a_var, '[ a_var inside a_func() ]')

a_func()
print(a_var, '[ a_var outside a_func() ]')
