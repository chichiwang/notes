"""Module level docstring.

Module level docstrings should appear at the top of the file before any other statements. This will appear when you request help() on the module.

Function level docstrings will appear when you request help() on a module method.

Triple quotes are used to create multi-line strings. These strings are conventionally used as documentation for functions.

Single lines also use the triple-quotes as seen on L12. This is so they can later be broken out into multiple lines.

Google style docstrings with arguments list and return value on L25
"""
import sys
from urllib.request import urlopen

default_url = 'http://sixty-north.com/c/t.txt'

def fetch_words(url):
    """Fetch a list of words from a URL."""
    with urlopen(url) as story:
        story_words = []
        for line in story:
            line_words = line.decode('utf-8').split()
            for word in line_words:
                story_words.append(word)
    return story_words


def print_items(items):
    """Prints items one per line.

    Prints individual elements of any iterable collection on a new line.

    Args:
        items: an iterable series of printable items.

    Returns:
        None
    """
    for item in items:
        print(item)


def main(requested_url):
    """Print each word from a text document from a URL.

    Args:
        url: The URL of a UTF-8 text document.
    """
    url = requested_url or default_url
    words = fetch_words(url)
    print_items(words)


if __name__ == '__main__':
    main(len(sys.argv) > 1 and sys.argv[1])
