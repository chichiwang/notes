"""Module for demonstrating generator execution."""

def take(count, iterable):
    """Take items from the front of an iterable

    Args:
        count: The maximum number of items to retrieve.
        iterable: The source series.

    Yields:
        At most 'count' items from 'iterable'.
    """

    counter = 0
    for item in iterable:
        if counter == count:
            return
        counter += 1
        yield item


def distinct(iterable):
    """Return unique items by eliminating duplicates.

    Args:
        iterable: The source series.

    Yields:
        Unique elements in order from 'iterble'.
    """
    seen = set()
    for item in iterable:
        if item in seen:
            continue
        seen.add(item)
        yield item


def run_pipeline():
    items = [3, 6, 6, 2, 1, 1]
    for item in take(3, distinct(items)):
        print(item)

if __name__ == '__main__':
    run_pipeline()
