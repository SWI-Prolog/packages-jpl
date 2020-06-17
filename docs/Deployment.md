# Deploying for users

To use JPL to embed Prolog in Java or Java in Prolog, at least three libraries need to be accessible:

* **Native library `jpl.dll/libjpl.so/libjpl.dylib`** (for Windows/Linux/MacOS respectively) implementing the interface between Java and C code. It must be found by the Windows/Linux/MacOS kernel.
    * In Windows, `jpl.dll` can go in any folder on your `PATH`; perhaps `%SWI_HOME_DIR%\bin` or your Windows' system folder.
    * In Linux, `libjpl.so` is generally  found in `/usr/lib/swi-prolog/lib/x86_64-linux/`. 
        * If needed, its dir should be in the `LD_LIBRARY_PATH` env variable.
    * In MacOS, `libjpl.dylib` may be in `/usr/local/lib/swipl-x.y.z/lib/x86_64-darwin17.7.0/libjpl.dylib`. 
        *  If needed, its dir should be in the `DYLD_LIBRARY_PATH` env variable.
* **JAR file `jpl.jar` implementing the Java API**. This file must be found by any Java VMs (and compilers) used with JPL. 
     * Would generally be in `SWI_HOME_DIR/lib/`.
     * The JAR has to be in the `CLASSPATH` or obtained automatically as a [Maven dependency](DelopymentJava).
* **Prolog source module file `jpl.pl` implementing the Prolog API**. This file will be used by the SWI-Prolog engines (either standalone or created by JPL from Java).
    * Would generally be in `$SWI_HOME_DIR/library`

Depending on your OS (Linux, Windows, MacOS), refer to the specific instruction details.

If you are interested to modify or develop JPL further, then you need a complete SWIPL+JPL setup from scratch. See the "Developing JPL" section for more information on that.

