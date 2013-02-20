Libraries for the Impromptu livecoding environment
-----------------------------------------

http://impromptu.moso.com.au/
(note that pc-ivl-lib.scm and xml_lib.scm are taken directly from the Impromptu codebase)

To load the libraries just customize the loader.scm file with you libraries location, eg:

(define *sys:home_libs* "/Users/mac/code/impromptu/_libs/")

Then dump the loader.scm file on your mac's Application Support/Impromtu so that it's loaded at startup. Have fun!



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