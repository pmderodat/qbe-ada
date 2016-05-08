QBE-Ada
=======

This repository contains an library to easily create
[QBE](http://c9x.me/compile/) programs in Ada. Note that this is in the early
stages of its development.

TODO: put an Hello world! example here once there is something interesting to
show.


Build
-----

In order to build this project, you need to install:

* an Ada 2012 compiler, a recent `gnat` Debian package will do the job, for
  instance;
* [GPRbuild](https://github.com/AdaCore/gprbuild).

Once they are available, just run from this directory:

```sh
gprbuild -Pqbe -p
```

The library will get generated in the `lib` subdirectory.


Testing
-------

Testcases are present in the [`tests`](tests/) subdirectory. Each testcase is a
mere Ada main procedure using the library that prints a QBE program on the
standard output. There is no automated testsuite yet, though.
