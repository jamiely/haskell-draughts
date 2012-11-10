# Intro

An attempt to learn Haskell better by implementing the game [Draughts](http://en.wikipedia.org/wiki/Draughts).

# Media
* 26be8a2 - http://youtu.be/34AEcHeJ6EE

# Discussion

I began this several months after a class I took on functional
programming in Haskell. Learning Haskell was a huge eye opener in terms
of thinking about programming and seeing what is out there. 

I've been working on it off (mostly off) and on since then, and finished
a playable implementation the first week of November 2012. Especially
after having dropped it and started it back up so many times in that
period, I've realized this implementation is very poor. Not only is it
non-idiomatic, but I have not taken full advantage of the type system
and functional aspects of the language to help complete it. 

Although the game is fairly trivial, there were areas of implementaiton 
that took me longer than expected, just trying to work out the logic. I
think that is the hallmark of bad or poorly organized code.

It would be nice at some point to get this code reviewed by some more
practiced Haskell programmers, and then attempt a rewrite. I have ideas
about how to utilize the type system to make the code more simple. Some
of the areas of logic might also be a good area to use GADTs, which I
recall are useful for (here my terminology may be lacking) type-check on
constructors. 

