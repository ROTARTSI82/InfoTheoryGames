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