# What is Flux.Collections

Flux.Collections is a set of functional data structures.

## Hash Array Mapped Trie (Map and Set)

**HamtMap** and **HamtSet** are both based on the same idea of using a trie and the keys' hash to store the key-value pairs. Clojure's map is a well known implementation of a Hamt.

Both have a **O(log<sub>32</sub>)** or **O(log<sub>64</sub>)** (depending on the prefix size used at compile time) time complexity for insert, find and delete operations.

## SkewBinaryRandomAccessList[^1]

This is a random access list that supports the list operations const, head, tail, but allows **O(log<sub>2</sub>)** access to any element by its index.

## Queues[^1]

**Queue** and **RealTimeQueue** are just queues. **Queue** has a **O(1)** amortized time for inserting and popping items, while **RealTimeQueue** has a **O(1)** time for the same operations. However in a long sequence of operations, Queue should have a better overall performance than it's real time counterpart.

## SkewBinomialHeap[^1]

It's a heap (priority queue) using an idea similar to that of the SkewBinaryRandomAccessList

## Stream[^1]

**Stream** is a lazy list implementation.

[^1]: Based on Chris Okasaki's "Purely Functional Data Structures"