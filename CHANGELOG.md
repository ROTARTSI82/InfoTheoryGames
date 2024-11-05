# Revision history for Hangman

## 0.1.0.0 -- 2024-11-02

* Incomplete. Can calculate entropy of guessing a letter.

## 0.1.0.1 -- 2024-11-02

* Almost complete. IO stuff to get word from user. Maximizes entropy to choose best letter to guess. 
Runs recursively until word is found. Still needs list of words from somewhere.

## 0.1.0.2 -- 2024-11-03

* Bug fixed. Did not filter potential list of words while running.

## 0.1.0.3 -- 2024-11-03

* Random Haskell fun. Code is now more cursed, but also variables have real names.

## 0.1.1.0 -- 2024-11-03

* Complete! List of words added. Works well for short words, but first guess takes like 3x longer for
each new character added.

## 0.2.0.0 -- 2024-11-04

* Huge! (and I mean huge) optimizations. Like crazy fast now (relative to before). It probably can
be optimized more, but it's probably better to first implement word frequency + lying.

## 0.2.0.1 -- 2024-11-04

* Small edits.

## 0.2.1.0 -- 2024-11-04

* Added word frequency. The results seemed to include a lot of random letters, so I added a filter for
the k most popular words (currently set to 50k out of the original 333k words). Most of these still do
not make any sense, but I'm not sure if I can set the filter lower? I think it's because the data is
based off of google search results or something. 

## 0.3.0.0 -- 2024-11-04

* Lying detection! Instead of filtering the list of potential candidates by the user input, it partitions
the list into two sublists, the first being truth, and the second being lie. Then, it only keeps parititoning
the first list, but it can just filter down the others (since you can only lie once). This repeats till there
is one word left.