"""Demonstrate scope"""
count = 0

def show_count():
    print("Count = ", count)


def set_count(c):
    global count # let the interpreter know to use the global count
    count = c
