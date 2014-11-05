Current buildstatus: [![Build Status](https://travis-ci.org/schutm/wakala.png)](https://travis-ci.org/schutm/wakala)

wakala
======
wakala is a proof of concept to investigate and show how easy it is to
create a websocket-to-socket proxy using Erlang. The websocket part can
be used from the webserver, as shown by the example. Using some excellent
libraries the code invovled is mostly javascript for the example code.
The proxy itself consist of almost no code at all, nevertheless it is
able to handle thousands of connections and is crash proof.


Installation
------------
Add the following to your rebar.config:

```
{deps, [
    {wakala, ".*",
     {git, "https://github.com/schutm/wakala.git", {branch, "master"}}}
]}
```

The [example](example) can be run by executing the start scripts in the example
directory.


Bug tracker
-----------
[Open a new issue](https://github.com/schutm/wakala/issues) for bugs
or feature requests. Please search for existing issues first.

Bugs or feature request will be fixed in the following order, if time
permits:

1. It has a pull-request with a working and tested fix.
2. It is easy to fix and has benefit to myself or a broader audience.
3. It puzzles me and triggers my curiosity to find a way to fix.


Acknowledgements
----------------
This proxy was easy to implement due to the excellent
[cowboy](https://github.com/ninenines/cowboy) server.


Contributing
------------
Anyone and everyone is welcome to [contribute](CONTRIBUTING.md).


License
-------
This software is licensed under the [ISC License](LICENSE).
