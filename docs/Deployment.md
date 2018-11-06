# JPL - Deployment (for use)

To use JPL, three libraries need to be deployed:

 * Native library `jpl.dll/.so/.dylib` must be found by the Windows/Linux/MacOS kernel:
    * In Windows, can go in any folder on your `PATH`; perhaps `%SWI_HOME_DIR%\bin` or your Windows system folder.
    * In Linux, it generally in `/usr/lib/swi-prolog/lib/amd64/libjpl.so`
    * In MacOS, it can install in `/usr/local/lib/swipl-7.7.19/lib/x86_64-darwin17.7.0/libswipl.dylib`
 * Java JAR file `jpl.jar` must be found by any Java VMs (and compilers) used with JPL; one possibility is to put it on your global `CLASSPATH`.
 * Prolog file `jpl.pl` is a Prolog source module, and must be found by any SWI-Prolog engines used with JPL
    * Would generally be in `%SWI_HOME_DIR%\library`

Depending on your OS (Linux, Windows, MacOS), refer to the specific instruction details.

If you are interested to modify or develop JPL further, then you need a complete SWI+JPL setup from scratch. See tutorial guide on how to develop JPL further.