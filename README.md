# EECS662-Final-Project

## Background Information
The fundamental goal for our course on programming languages was to develop a wider vocabulary for discussing what programming languages, and the various programs written in them, do. Part of this learning process was establishing a toolset of concepts that we can utilize to extend, or elaborate on, a preexisting programming language. In the case of this final project that we are presenting to you today, this “extension” manifests itself as a new function class common in many other programming languages - lists. Lists are a fundamental component to any robust functional language. They can represent other more nuanced data structures, such as strings; that is, you could represent a string as a list of characters. It is a common phenomena as a programmer to deal with ordered collections of objects, and lists help us manage that. We thought that implementing lists would be difficult enough to be challenging and interesting for our group, but not too difficult so as to be unachievable. It also seemed like a fairly important feature for a language to have. 

## Methods & Appraoch
Our list implementation is based on the “pair” construct that we learned to implement in lecture. Pairs, in this context, are sets of terms of size two; terms are generic expressions, values, or even other pairs. This formal construct allows us to build nested pairs that, given some helper functions and recursion, can function as the traditional ‘list.’

`[ 1, 3, 2, 7, 9 ] → (1, (3, (2, (7, 9))))`

This is the basic idea, but we were limited in some respects. Our lists in particular are restricted to a single type, which is not always the case in other languages. We also have limited the functions that can be applied to lists, such as only being able to prepend or not being able to find values in the list.

## Conclusions
In conclusion, we believe we were successful in the basic implementation of pair-based lists through unit testing and the analysis of multiple edge cases. This included correct functionality for prepending to the list and the proper list type returned with our typeof method.
