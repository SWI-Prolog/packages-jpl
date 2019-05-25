# Deploying for users

To use JPL, three libraries need to be deployed:

 * Native library `jpl.dll/libjpl.so/libjpl.dylib` (for Windows/Linux/MacOS respectively) implementing the interface between Java and C code. It must be found by the Windows/Linux/MacOS kernel.
    * In Windows, it can go in any folder on your `PATH`; perhaps `%SWI_HOME_DIR%\bin` or your Windows' system folder.
    * In Linux, it is generally  found in `/usr/lib/swi-prolog/lib/amd64/libjpl.so`
    * In MacOS, it can install in `/usr/local/lib/swipl-7.7.19/lib/x86_64-darwin17.7.0/libjpl.dylib`
 * JAR file `jpl.jar` implementing the Java API. This file must be found by any Java VMs (and compilers) used with JPL. 
     * One possibility is to put it on your global `CLASSPATH`.
     * It will generally be found if `SWI_HOME_DIR` is correctly setup.
 * Prolog source module file `jpl.pl` implementing the Prolog API. This file will be used by the SWI-Prolog engines (either standalone or created by JPL from Java).
    * Would generally be in `%SWI_HOME_DIR%\library`

Depending on your OS (Linux, Windows, MacOS), refer to the specific instruction details.

If you are interested to modify or develop JPL further, then you need a complete SWI+JPL setup from scratch. See the "Developing JPL" tutorial guide in this documentation.

## Oracle Java JDK vs OpenJDK

The current guide/documentation has been produced using the [Oracle Java](https://www.oracle.com/java/) SE 8.

However, [others have reported using it](https://github.com/ssardina-research/packages-jpl/issues/23) successfully with OpenJDK, which can be downloaded from [AdaptOpenJDK](https://adoptopenjdk.net/).  Then, select the **OpenJ9** as JVM (using the Hotspot may yield a fatal error).

You can read a comparison between Oracle JDK and OpenJDK [here](https://www.baeldung.com/oracle-jdk-vs-openjdk).
