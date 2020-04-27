# Deploying for users - on Linux

To use JPL under Linux one must have the following in place (here showing the locations under Linux Mint distribution under packages `swi-prolog-nox` and `swi-prolog-java`):

* A SWIPL install containing the core SWI system, which includes:
   * A SWIPL home directory (`SWI_HOME_DIR`), located at `/usr/lib/swi-prolog/` and containing the whole SWIPL core system, including a booting `.prc` file (`$SWI_HOME_DIR/boot64.prc`).
       * The "home" of SWIPL can be queried via [`current_prolog_flag(home, H)`](https://www.swi-prolog.org/pldoc/man?predicate=current_prolog_flag/2)
   * The C Native core SWIPL library `/usr/lib/libswipl.so`.
   * The set of C Native SWIPL libraries for each package (files `.so`), located in `/usr/lib/swi-prolog/lib/amd64/`
* The JPL package, which includes:
   * The C Native JPL Library `libjpl.so` at `$SWI_HOME_DIR/lib/amd64/libjpl.so`.
   * The Java API JAR file `jpl.jar` in `$SWI_HOME_DIR/lib/jpl.jar`.
   * The Prolog API as an SWIPL source module `jpl.pl` at `$SWI_HOME_DIR/library`.
   
## Which SWIPL version to (not) use?
  
JPL is **generally distributed with official Linux**. For example, in Ubuntu-based systems, JPL is provided via package `swi-prolog-java`. That package includes the C library `libjpl.so`, the Java API `jpl.jar`, the Prolog module `jpl.pl` as well as all associated documentation.

However, the **official packages are often out-of-date**. For Debian-based systems (Debian, Ubuntu, Mint, ...) you can get the latest stable and development versions via [this PPAs](http://www.swi-prolog.org/build/PPA.txt) provided directly by SWI-Prolog.

Use either SWIPL stable version 7.6.4 (available in standard Linux repos) or compile & install 8.1.x from [SWI-devel repo](https://github.com/SWI-Prolog/swipl-devel) using CMAKE.
   * **Note:** The official stable SWI 8.0.x versions (as of Jan 2020) have issues with the `libswipl.so/dll/dylib` library and makes JPL crash; see [issue](https://github.com/ssardina-research/packages-jpl/issues/21). It has been fixed in the git repo but will only show up with versions 8.1.x.
   
## Configuring environment variables

When embeeding SWIPL into a Java, one may needs to "tell" the Java application the right information, via environment variables, so that SWIPL is initialized properly and can find all resources.

In general, if the Java application will use the default executable of the system (i.e., the one that runs when executing `swipl`), then you only need to set-up `CLASSPATH` to include `jpl.jar` and possibly `LD_PRELOAD` to point to your active SWIPL `libswipl.so` library to avoid run-time errors. The executable has a pointer to the right information to initialize; see [here](https://www.swi-prolog.org/FAQ/FindResources.html).

However, if your Java application will use an SWIPL & JPL version that is _not_ the exectuable default.

### Using stable distribution versions of SWIPL

If the **Linux distribution install** of SWIPL & JPL is not the executable default but the one to be used, set-up the following environment variables:

      SWI_HOME_DIR=/usr/lib/swi-prolog/   # if default binary not pointing to this version
      LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/amd64/     # to find all .so, including libjpl.so
      CLASSPATH=/usr/lib/swi-prolog/lib/jpl.jar
      LD_PRELOAD=/usr/lib/libswipl.so  # see below for explanation

or in one line (for IDE Run configurations, for example):

    CLASSPATH=/usr/lib/swi-prolog/lib/jpl.jar;LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/amd64/;LD_PRELOAD=/usr/lib/libswipl.so;SWI_HOME_DIR=/usr/lib/swi-prolog/

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

If you want to run your application against a development tree of SWIPL & JPL that is not yet installed, then refer to [Developing JPL](TutorialDeveloping.md) guide.


## Troubleshooting

### Pre-loading `libswipl.so`

The [libray preloading](https://blog.cryptomilk.org/2014/07/21/what-is-preloading/) of `libswipl.so` is often necessary to avoid the following run-time error (check [this](https://answers.ros.org/question/132411/unable-to-load-existing-owl-in-semantic-map-editor/), [this](https://github.com/yuce/pyswip/issues/10) and [this](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=690734) posts):

      ERROR: /usr/lib/swi-prolog/library/process.pl:53:
         /usr/lib/swi-prolog/library/process.pl:53: Initialization goal raised exception:
         '$open_shared_object'/3: /usr/lib/swi-prolog/lib/amd64/process.so: undefined symbol: Sfilefunctions

###  [FATAL ERROR: Could not find system resources].

It is not finding the correct `libswipl.so`. 
