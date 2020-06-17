# Using the live development tree

Suppose you want to run the SWIPL (and JPL) system that is currently under the development source tree and not yet installed in the system. In this example, we first save the directory where SWIPL was built by CMAKE in environment variable `SWI_BUILD` as follows:

```bash
export SWI_BUILD=/home/ssardina/git/soft/prolog/swipl-devel.git/build/
```


We can now start SWIPL from CLI as follows:

```bash
[ssardina@Thinkpad-X1 build]$ $SWI_BUILD/src/swipl -x $SWI_BUILD/home/boot.prc -F swipl --home=$SWI_BUILD/home 
Welcome to SWI-Prolog (threaded, 64 bits, version 8.3.1-34-g9adbe8ff1-DIRTY)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

    CMake built from "/home/ssardina/git/soft/prolog/swipl-devel.git/build"

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- 
```

The option `-F swipl` in the source tree will execute file `swipl.rc` which sets-up all the search paths for this particular development and uninstalled version. 

Next, if we want to use JPL, we need to set-up two variables:

```
export CLASSPATH=$SWI_BUILD/packages/jpl/src/main/java/jpl.jar:$CLASSPATH
export LD_LIBRARY_PATH=/home/ssardina/git/soft/prolog/swipl-devel.git/build/packages/jpl:$LD_LIBRARY_PATH
```

The setting of `CLASSPATH` is necessary so that `jpl.pl` has access to the JPL JAR compiled in the development tree (to access JAVA from Prolog).

The setting of `LD_LIBRARY_PATH` is necessary so that SWIPL can find the just compiled but uninstalled `libjpl.so`. This is because the "temporary" home dir in the development source tree does not include the usual `lib/` directory with the compiled native libraries: they are all under `$SWI_BUILD/packages` when compiled. 

Finally, the use of some packages may require:

```bash
export LD_PRELOAD=$SWI_BUILD/src/libswipl.so
```


## Using SWIPL & JPL embedded

When calling SWIPL & JPL from an _embedded Java application_ (instead of just calling SWIPL from CLI), one will need to initialize the SWIPL engine from the Java code so that the SWIPL in the development source tree is used.  

To do so, we can first define the environment variables as folows:

```bash
export LD_LIBRARY_PATH=$SWI_BUILD/packages/jpl:$LD_LIBRARY_PATH
export SWI_HOME_DIR=$SWI_BUILD/home
export SWIPL_BOOT_FILE=$SWI_BUILD/home/boot.prc
export CLASSPATH=$SWI_BUILD/packages/jpl/src/main/java/jpl.jar:$CLASSPATH
export LD_PRELOAD=$SWI_BUILD/src/libswipl.so
```

Using such variables, we can use the following Java code to initialize SWIPL correctly:

```java
public static final String swi_home = System.getenv("SWI_HOME_DIR"));
public static final String swi_startup  = System.getenv("SWIPL_BOOT_FILE"));
public static final String swi_exec = String.format("%s/../src/swipl", home);

String init_swi_config =
        String.format("%s -x %s -F swipl --home=%s -f none -g true -q",
            swi_exec, swi_startup, swi_home);
JPL.setDefaultInitArgs(init_swi_config.split("\\s+"));	# initialize SWIPL engine
JPL.init()
```

Again, the option `-F swipl` will execute file `swipl.rc` to sets-up all the search paths for this particular development and uninstalled version: it won't be necessary for installed SWIPL systems (e.g., in `/usr/local`).



