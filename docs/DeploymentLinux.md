# Deploying for users - on Linux

To use JPL under Linux one must have the following in place (here showing the locations under Linux Mint distribution under packages `swi-prolog-nox` and `swi-prolog-java`):

* A SWIPL install containing the core SWI system, which includes:
   * A SWIPL [home directory](https://www.swi-prolog.org/pldoc/man?section=findhome), located at `/usr/lib/swi-prolog/` and containing the whole SWIPL core system, including a booting `.prc` file (e.g., `boot.prc`).
       * The "home" of SWIPL can be queried via [`current_prolog_flag(home, H)`](https://www.swi-prolog.org/pldoc/man?predicate=current_prolog_flag/2)
   * The C Native core SWIPL library `/usr/lib/swi-prolog/lib/x86_64-linux/libswipl.so`.
   * The set of C Native SWIPL libraries for each package (files `.so`), located in `/usr/lib/swi-prolog/lib/x86_64-linux/`
* The JPL package, which includes:
   * The C Native JPL Library `libjpl.so` at `$SWI_HOME_DIR/lib/x86_64-linux/libjpl.so`.
   * The Java API JAR file `jpl.jar` in `$SWI_HOME_DIR/lib/jpl.jar`.
   * The Prolog API as an SWIPL source module `jpl.pl` at `$SWI_HOME_DIR/library`.
   
**_What does an embedded application ned to find?_**

* The Java API via `jpl.jar`. This needs to be in the `CLASSPATH`.
* All Prolog `.pl` core libraries, including `jpl.pl`. These are found relative to SWIPL's home: `$SWI_HOME_DIR/library`
* All C Native libraries `.so`, including `libjpl.so`. For that, `LD_LIBRARY_PATH` needs to be given.
* If doing Prolog-calls-Java, `libjvm.so`'s directory needs to be in `LD_LIBRARY_PATH` for the JVM shared objects to be found.

 
## Which SWIPL version to (not) use?
  
JPL is **generally distributed with official Linux**. For example, in Ubuntu-based systems, JPL is provided via package `swi-prolog-java`. That package includes the C library `libjpl.so`, the Java API `jpl.jar`, the Prolog module `jpl.pl` as well as all associated documentation. However, these distribution-based packages are **highly out-of-date**. There are two solutions to get more up to date SWIPL:

1. For Debian-based systems (Debian, Ubuntu, Mint, ...) you can get the latest stable and development versions via [this PPAs](http://www.swi-prolog.org/build/PPA.txt) provided directly by SWI-Prolog.
    * As of June 2020, it has 8.2.0 and JPL 7.6.0.

2. Manually compile & install from sources at [SWI-devel repo](https://github.com/SWI-Prolog/swipl-devel) using CMAKE. See instructions below.

   
## Configuring environment variables

When embeeding SWIPL into a Java, one may needs to "tell" the Java application the right information, via environment variables, so that SWIPL is initialized properly and can find all resources (`.pl` and `.so` libraries).

In general, if the Java application will use the default executable of the system (i.e., the one that runs when executing `swipl`), then you only need to set-up `CLASSPATH` to include `jpl.jar` and possibly `LD_PRELOAD` to point to your active SWIPL `libswipl.so` library to avoid run-time errors. The executable has a pointer to the right information to initialize; see [here](https://www.swi-prolog.org/FAQ/FindResources.html).

However, if your embedded application must use an SWIPL & JPL version that is _not_ the executable default in the system, one needs to provide the right paths to find SWIPL's home dir and SWIPL's C native `.so` libraries.

### Using stable distribution versions of SWIPL

If the **Linux distribution install** of SWIPL & JPL is not the executable default but the one to be used, set-up the following environment variables:

      SWI_HOME_DIR=/usr/lib/swi-prolog/   # if default binary not pointing to this version
      LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/x86_64-linux/   # to find all .so, including libjpl.so
      CLASSPATH=/usr/lib/swi-prolog/lib/jpl.jar # Java API
      LD_PRELOAD=/usr/lib/libswipl.so  # core SWIPL

or in one line (for IDE Run configurations, for example):

    CLASSPATH=/usr/lib/swi-prolog/lib/jpl.jar;LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/x86_64-linux/;LD_PRELOAD=/usr/lib/swi-prolog/lib/x86_64-linux/libswipl.so;SWI_HOME_DIR=/usr/lib/swi-prolog/

Notice that, in this case, library `libswipl.so` will be found automatically, as it is located in the standard system-wide library dir `/usr/lib`.

### Using locally compiled and installed version of SWIPL

Alternatively, if you have **compiled and installed** an SWIPL system, say, under directory `/usr/local/swipl-git/`, then the SWIPL home will be `/usr/local/swipl-git/lib/swipl/`, the executable binary will be `/usr/local/swipl-git/lib/swipl/bin/x86_64-linux/swipl` and the environment variables should be set-up as follows:

      SWI_HOME_DIR=/usr/local/swipl-git/lib/swipl/  # if binary exec not pointing to this SWIPL
      LD_LIBRARY_PATH=/usr/local/swipl-git/lib/swipl/lib/x86_64-linux/     # to find all .so, including libjpl.so
      CLASSPATH=/usr/local/swipl-git/lib/swipl/lib/jpl.jar
      LD_PRELOAD=/usr/local/swipl-git/lib/swipl/lib/x86_64-linux/libswipl.so  # see below for explanation

or in one line (for IDE Run configurations, for example):

    CLASSPATH=/usr/local/swipl-git/lib/swipl/lib/jpl.jar;LD_LIBRARY_PATH=/usr/local/swipl-git/lib/swipl/lib/x86_64-linux/;LD_PRELOAD=/usr/local/swipl-git/lib/swipl/lib/x86_64-linux/libswipl.so;SWI_HOME_DIR=/usr/local/swipl-git/lib/swipl/

## Using the development tree of SWI+JPL that is yet not installed?

If you want to run your application against a development source tree of SWIPL & JPL that is not yet installed, then refer to [Developing JPL](TutorialDeveloping.md) guide.



## Troubleshooting


### Cannot find `libjvm.so` (Prolog-calls-Java)

When loading JPL module in Prolog (Prolog-calls-Java):

```prolog
?- use_module(library(jpl)).
ERROR: /usr/lib/swi-prolog/library/jpl.pl:4243:
	'$open_shared_object'/3: libjvm.so: cannot open shared object file: No such file or directory
ERROR: /usr/lib/swi-prolog/library/jpl.pl:4243:
	/usr/lib/swi-prolog/library/jpl.pl:4243: Initialization goal raised exception:
	library `java' does not exist (Please add directory holding libjava.so to $LD_LIBRARY_PATH)
ERROR: Exported procedure jpl:jpl_c_lib_version/1 is not defined
true.
```

Do a `locate libjvm.so` to find where it is and add the path to `LD_LIBRARY_PATH` so that Java VM shard objects can be found:

```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/jvm/java-8-oracle/jre/lib/amd64/server/
```

### FATAL ERROR: Could not find system resources

System is unable to find the SWI Prolog framework, including failing to find library `libswipl.so`. See [here](https://www.swi-prolog.org/FAQ/FindResources.html)

Make sure `LD_LIBRARY_PATH` and `SWI_HOME_DIR` are correctly set (see above).


### Run-time error: `process.so: undefined symbol: Sfilefunctions`

You need to tell the system to pre-load SWI main library:

```bash
export LD_PRELOAD=/home/ssardina/git/soft/prolog/swipl-devel.git/build/src/libswipl.so
```

Check [this post](https://answers.ros.org/question/132411/unable-to-load-existing-owl-in-semantic-map-editor/) and [what is preloading?](https://blog.cryptomilk.org/2014/07/21/what-is-preloading/)


### Error "undefined symbol: PL_new_atom" when reading files #10

Check [this post](https://github.com/yuce/pyswip/issues/10).

This is resolved by pre-loading `libswipl.so` via `LD_PRELOAD` as above.

### ERROR source_sink `jar('jpl.jar')' does not exist

Some Prolog file is not able to find `jpl.jar` that it needs to call some class. We need to setup `CLASSPATH` to point to `jpl.jar` to be used.