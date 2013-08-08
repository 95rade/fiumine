# fiumine

Fiumine is a simple web radio station. It takes a folder full of music files
(FLAC, OGG, or MP3), shuffles them, and makes them available as an Ogg/Vorbis
stream over HTTP.  Ogg/Vorbis was chosen primarily for its excellent support 
across all major web browsers.  

The motivation for this project is primarily a means for me to write something
in Clojure in order to gain some practical experience.  I'm not sure what use, 
if any, this code has beyond my humble goal of learning a new programming 
language.  As of this writing it remains fairly unpolished.  Internally, the 
design is fairly modular.  There are independent components for composing an 
Ogg/Vorbis stream, publishing that stream to multiple subscribers, and servicing
subscribers via HTTP.  Its modular structure should lend itself well to hacking
if you have other devious use cases in mind.

Fiumine doesn't do anything special with regards to I/O.  All I/O and 
concurrency is fairly standard blocking and threading.  It's not clear that
async would buy much here, as I seem to be primarily CPU bound.  Testing with
Apache Bench on my laptop via loopback, I can get about 600 concurrent 
connections before players start stuttering which seems decent enough.

## Installation

You must have Java 6 or later installed on your system.  If you do not have 
Leiningen installed, download and install here: http://leiningen.org/.

Fiumine depends on two externally installed programs: sox and oggenc.  They are
easily procured through the typical package managers. MP3 support for sox may
be a separately installed package in your OS distribution.

Get the project from GitHub:

    $ git clone https://github.com/chrisrossi/fiumine.git
    $ cd fiumine

Fiumine depends on a Clojure library, Marshal, that has an unreleased bug fix, 
so it must be included by hand for the time being:

    $ mkdir checkouts
    $ cd checkouts
    $ git clone https://github.com/russellc/Marshal.git
    $ cd ..

Leiningen will install any other dependencies the first time you run it:

    $ lein run /path/to/music/folder 8000

Fiumine will recursively find music files in subfolders of your music folder, 
then start a service listening on the port you specify (8000 above).  Point
your web browser or anything else that can play music from a web stream at:
http://localhost:8000/

That's it!

## License

Copyright © 2013 Chris Rossi

Distributed under the Eclipse Public License, the same as Clojure.
