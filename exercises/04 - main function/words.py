"""
When you import this module, the import statement in the module global scope executes immediately. The if-condition in the module global scope also executes immediately.

Modified from the last exercise:
* Split print_items() out from fetch_words()
* Added an argument to fetch_words() to allow url to be passed in
* Genericized variable names in print_items() to allow it to print any collection
* Added a main() function to be called with a requested url
* Set a default_url if no argument is provided from the command line, or call to main()
* Import sys module to read arguments to command line python calls
"""
import sys
from urllib.request import urlopen

default_url = 'http://sixty-north.com/c/t.txt'

def fetch_words(url):
    with urlopen(url) as story:
        story_words = []
        for line in story:
            line_words = line.decode('utf-8').split()
            for word in line_words:
                story_words.append(word)
    return story_words

def print_items(items):
    for item in items:
        print(item)

def main(requested_url):
    url = requested_url or default_url
    words = fetch_words(url)
    print_items(words)

if __name__ == '__main__':
    main(len(sys.argv) > 1 and sys.argv[1])
