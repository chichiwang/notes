# Intro to Algorithms
[Video Link](https://youtu.be/rL8X2mlNHPM)

The specific steps used to compute a given computation is called an [algorithm](../glossary/README.md#algorithm). Some algorithms are better than others, even if they produce equal results. Generally: the fewer steps it takes to compute, the better it is, but sometimes other factors are taken into account (like how much memory is used for a computation).

The term _algorithm_ comes from Persian polymath [Muḥammad ibn Mūsā al-Khwārizmī](https://en.wikipedia.org/wiki/Muhammad_ibn_Musa_al-Khwarizmi), who was one of the fathers of algebra. The crafting of efficient algorithms, a problem that existed before modern computers, led to a whole science around computation which evolved into the modern discipline of computer science.

One of the most storied algorithmic problems in all of computer science is [sorting](https://en.wikipedia.org/wiki/Sorting_algorithm). Computers sort all the time: searching for the cheapest airfare, arranging emails by most recently sent, or listing out contacts by last name, to name a few. There are [a large number of sorting algorithms](https://www.geeksforgeeks.org/sorting-algorithms/) - computer scientists have spent decades inventing algorithms for this task.

The relationship of _input size_ to the number of steps an algorithm takes to run characterizes the [complexity](../glossary/README.md#computational-complexity) of the algorithm. It gives an approximation of how fast or slow an algorithm is going to be. Computer scientists express this order of growth in what's called [big O notation](../glossary/README.md#big-o-notation).

Another interesting category of algorithm is [graph search](../glossary/README.md#graph-traversal). A [graph](../glossary/README.md#graph) is a network of nodes connected by lines. The classic algorithmic solution to finding the shortest path between two nodes in a graph was invented by [Edsger Wybe Dijkstra](https://en.wikipedia.org/wiki/Edsger_W._Dijkstra), one of the great minds in computer science practice and theory: [Dijkstra's algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm). Dijkstra's original algorithm, conceived in 1956, had a complexity of O(n<sup>2</sup>), or the number of nodes in the graph squared. This algorithm was improved a few years later to O(n log n + l) where `n` is the number of nodes in the graph and `l` is the number of lines.

Much like search algorithms, there are numerous graph traversal algorithms. These algorithms are utilized by software such as maps applications to calculate the best route betwen two points.

Algorithms are in use everywhere - the modern world would not be possible without them. A central part of being a computer scientist is leveraging existing algorithms and writing new ones when needed.

| [Previous: Statments and Functions](../12/README.md) | [Table of Contents](../README.md#table-of-contents) | Next |
