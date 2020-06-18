# Initializing the Prolog engine

Although the `org.jpl7.JPL` class provides a number of methods for initializing the Prolog engine from within Java, their use is not usually necessary: Prolog will be _automatically initialized_ with default parameters at the first attempt to use it.

By default, the SWIPL install accessible via the _default executable_ (i.e., the one that runs when one types `swipl`) will be initialized. From it, [SWIPL will try to find/guess the resources](https://www.swi-prolog.org/FAQ/FindResources.html) and set-up the engine correctly. 

So, most users will rely on automatic initialization. However, the following is a template for explicitly initializing the Prolog engine:

```java
if (System.getenv("SWI_HOME_DIR") != null ||
        System.getenv("SWI_EXEC_FILE") != null ||
        System.getenv("SWIPL_BOOT_FILE") != null) {
   String init_swi_config =
            String.format("%s %s %s -g true -q --no-signals --no-packs",
                    System.getenv("SWI_EXEC_FILE") == null ? "swipl" :
                            System.getenv("SWI_EXEC_FILE"),
                    System.getenv("SWIPL_BOOT_FILE") == null ? "" :
                            String.format("-x %s", System.getenv("SWIPL_BOOT_FILE")),
                    System.getenv("SWI_HOME_DIR") == null ? "" :
                            String.format("--home=%s", System.getenv("SWI_HOME_DIR")));
    System.out.println(String.format("\nSWIPL initialized with: %s", init_swi_config));

    JPL.setDefaultInitArgs(init_swi_config.split("\\s+"));    // initialize SWIPL engine
} else
    System.out.println("No explicit initialization done: no SWI_HOME_DIR, SWI_EXEC_FILE, or SWIPL_BOOT_FILE defined");

//JPL.setTraditional(); // most often not used
JPL.init();
System.out.println("Prolog engine actual init args: " + Arrays.toString(Prolog.get_actual_init_args()));
``` 

(For an explanation on the various options used (`-x`, `--home`, `-f`, etc.) pelase refer to the set of available [CLI options](https://www.swi-prolog.org/pldoc/man?section=cmdline).)

The above template will initialize the Prolog engine if any of the three environment variables `SWI_HOME_DIR`, `SWI_EXEC_FILE`, or `SWIPL_BOOT_FILE` are provided. 

The example `system.Init` (located in `$SWI_HOME_DIR/doc/packages/examples/jpl/java/system`) uses the above template and can be run as follows:

```bash
LD_LIBRARY_PATH=/usr/local/swipl-git/lib/swipl/lib/x86_64-linux/   \
   SWI_HOME_DIR=/usr/local/swipl-git/lib/swipl \
   SWI_EXEC_FILE=/usr/local/swipl-git/lib/swipl/bin/x86_64-linux/swipl \
   SWIPL_BOOT_FILE=/usr/local/swipl-git/lib/swipl/boot.prc \    
    java -cp ../:/usr/local/swipl-git/lib/swipl/lib/jpl.jar system.Init  
```


Note also that one may need to tell your development environment (e.g., IntelliJ or ECLIPSE) or applications to use the correct `jpl.jar`, by correctly setting the `CLASSPATH` variable.

Finally, if you want to call Java from Prolog using JPL, you need to add the directory holding the JVM shared objects to the dynamic linker search path:
 
 
```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/jvm/java-11-openjdk-amd64/lib/server/libjvm.so
```

