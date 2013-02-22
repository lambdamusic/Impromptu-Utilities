Impromptu Livecoding Libraries
=============
Scheme functions for speedier Impromptu livecoding.

###What is Impromptu?

Impromptu (http://impromptu.moso.com.au/) is an OSX programming language and environment for composers, sound artists, VJ's and graphic artists with an interest in live or interactive programming. Impromptu is a Scheme language environment, a member of the Lisp family of languages. Impromptu is used by artist-programmers in livecoding performances around the globe.

###What's in the libraries? 
A bunch of scheme functions that can make your life easier when you're writing code for Impromptu. Stuff for operating with beats, playing music, and other utils too (note that pc-ivl-lib.scm and xml_lib.scm are taken directly from the Impromptu codebase).

###How to load? 
To load the libraries just customize the loader.scm file with you libraries location, eg:

``(define *sys:home_libs* "/Users/mac/code/impromptu/_libs/")``

Then dump the ``loader.scm`` file on your mac's ``Application Support/Impromptu/`` folder so that it's loaded at startup. Have fun!



Changelog
-----------------------------------------


2012-09-30
Added new metronome-ticker function

2012-09-29
Reorganized the libs files and namespaces (mu: for music, etc..)

2012-04-15
Added 'play-au' macro 
http://www.michelepasin.org/musicblog/2011/03/29/an-alternative-to-the-play-macro-iplay-and-with-instrument/

2012-03-11
First commit
