"""Module demonstrating the use of a generator function to iterate over an infinite sequence."""

def lucas():
    """Returns the values of the Lucas Series.

    Returns:
        2, 1, then the 2 previous results added together into infinity.
    """

    yield 2
    a = 2
    b = 1
    while True:
        yield b
        a, b = b, a + b


def print_first_n_lucas(n):
    """Prints the first n values of the Lucas Series.

    Args:
        n: The number of Lucas Series values to print.
    """

    for idx, val in enumerate(lucas()):
        if idx == n:
            break
        print(val)


if __name__ == '__main__':
    print_first_n_lucas(10)
