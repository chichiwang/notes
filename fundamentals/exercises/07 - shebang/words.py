#!/usr/bin/env python3.7
"""Module level docstring.

Typical Python 3 shebangs use the unix env program to locate Python 3 on your environment path variable. This is, importantly, compatible with Python virtual environments.

On unix systems, we have to mark our script as executable using chmod:
chmod +x words.py

We can then run our script directly on the command line:
./words.py http://sixty-north.com/c/t.txt
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
    main(len(sys.argv) > 1 and sys.argv[1]) # The 0th arg is the module filename
