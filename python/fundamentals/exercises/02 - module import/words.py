"""
When you import this module, the import statement in the module global scope executes immediately.

The function fetch_words() does not execute immediately, but can be called by the importing module.
"""
from urllib.request import urlopen

def fetch_words():
    with urlopen('http://sixty-north.com/c/t.txt') as story:
        story_words = []
        for line in story:
            line_words = line.decode('utf-8').split()
            for word in line_words:
                story_words.append(word)

    for word in story_words:
        print(word)
