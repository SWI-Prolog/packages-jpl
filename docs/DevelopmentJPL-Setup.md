# Setting up SWIPL & JPL for development

The second thing to understand is that JPL is just a package on the overall SWI Prolog system. So, to compile the whole JPL, one needs the core SWIPL system, as the JPL C component (`libswipl.so`) links against the core SWI library (e.g., `libswipl.so`):

## Preparing

First, let us make sure we have all the necessary libraries and tools to compile the system:

```bash
sudo apt-get install \
        build-essential autoconf curl chrpath pkg-config \
        ncurses-dev libreadline-dev libedit-dev \
        libunwind-dev \
        libgmp-dev \
        libssl-dev \
        unixodbc-dev \
        zlib1g-dev libarchive-dev \
        libossp-uuid-dev \
        libxext-dev libice-dev libjpeg-dev libxinerama-dev libxft-dev \
        libxpm-dev libxt-dev \
        libdb-dev \
        libpcre3-dev \
        libyaml-dev \
        openjdk-11-jdk junit \
        make ninja-build \
        cmake
```

Since we want to get JPL it is mandatory to have:
- `openjdk-11-jdk` or any package that can provide `java` and the compiler `javac`. (Run `javac -version` to test).
- `junit`, for running Java unit testing.

As explained in `fnogatz`'s [swivm](https://github.com/fnogatz/swivm) repo, if you want to reduce resources, the following packages are optional:

- `libreadline-dev` and `libedit-devl`: Without, you do not have history feature in SWIPL interpreter.
- `unixodbc-dev`: Without, you have no ODBC database connectivity (e.g., MySQL)
- `libssl-dev`: Without, you have no SSL (and HTTPS) support.
- `libgmp-dev`: Without, you lack unbounded integer support, rational numbers, good random number generators, etc.
- `libarchive-dev`: Without, you can not unpack and install add-ons.
- `libpcre3-dev`: Without, you have no regular expression support ([library(pcre)](http://www.swi-prolog.org/pldoc/doc/_SWI_/library/pcre.pl)).
- `libyaml-dev`: Without, you have no YAML support ([library(yaml)](http://www.swi-prolog.org/pldoc/doc/_SWI_/library/yaml.pl)).


Second, let us clone the complete SWIPL system:

```bash
$ git clone git@github.com:SWI-Prolog/swipl-devel.git 
```

Next, get into the system and set-up all the packages (including JPL): 

```bash 
$ cd swipl-devel/  
$ git submodule update --init // get sources for all modules
```

If you don't want all the packages that come with SWIPL, then at least you need JPL and unit testing:

```bash 
$ git submodule update --init packages/jpl        # get JPL source code
$ git submodule update --init packages/plunit     # get SWIPL unit testing
```


Next, make sure that any SWI Prolog system install that you may have (e.g., the one that comes with the Linux distro) does not interfere with your development install. To do so, make sure you do not have `LD_PRELOAD` or `LD_LIBRARY_PATH` pointing to your local install version of `libswipl.so` (e.g., `/usr/lib/libswipl.so`). Otherwise, your development will use that system install, which is not what you want:

```bash
unset LD_LIBRARY_PATH LD_PRELOAD SWI_HOME_DIR   # don't use SWI installs
```

Also, JPL will be compiled with SWIPL _only if Java and JNI are found_, otherwise JPL will just be skipped by CMAKE. The latter (JNI) wil be found by CMAKE only when `JAVA_HOME` is set to the root of the Java to be used:

```bash
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64/
```


## Compiling & Installing SWIPL with JPL

Everything is ready. Now, **build the SWIPL system**  (SWIPL + JPL + PLUNIT) using CMAKE (with install in`/usr/local/swipl-git`):

```bash
$ mkdir build
$ cd build

# for make
$ cmake -DCMAKE_INSTALL_PREFIX:PATH=/usr/local/swipl-git -DINSTALL_DOCUMENTATION=OFF ..
$ make -j 8   

# for ninja
$ cmake -DCMAKE_INSTALL_PREFIX:PATH=/usr/local/swipl-git -DINSTALL_DOCUMENTATION=OFF ..
$ ninja -v -j 8
```

If compiled with no errors, you can **unit test** as follows:

```bash
$ ctest -V -R jpl:prolog_in_java  # run test of Java calling Prolog
$ ctest -V -R jpl:java_in_prolog  # run test of SWI calling Java
```

To **install** in the system at `/usr/local/swipl-git`:

```bash
$ sudo make install   # if compiled with make

$ sudo ninja install  # if compiled with ninja
```

If you want to have more than one SWIPL installed (your locally compiled system and the distribution one), you may want to use  `fnogatz`'s [swivm](https://github.com/fnogatz/swivm) to manage them.


## Update & Upgrade

To **update/upgrade** the SWIPL framework to the latest one:

```bash
$ cd swipl-devel
$ git pull
$ git submodule update --init

$ unset LD_LIBRARY_PATH LD_PRELOAD SWI_HOME_DIR   # to make sure no use of old SWI install

$ cd build        # already exists form previous set-up
$ cmake -C packages/jpl clean ..
$ make -j 8 // or ninja
$ sudo make install
```


