# JPL - Deployment - Installing & compiling JPL 

Updated: October 12, 2018

* Information on how to [use JPL in Windows and Linux](https://github.com/ssardina-research/packages-jpl/wiki/JPL-Under-Linux-and-Windows)
* Information how to [set-up JPL in MacOS](https://github.com/ssardina-research/packages-jpl/wiki/JPL-under-Mac-OS)


## General 

To use JPL, three libraries need to be placed where each can be found:

 * `jpl.dll/.so/.dylib` must be found by the Windows/Linux/MacOS kernel:
    * In Windows, can go in any folder on your `PATH`; perhaps `%SWI_HOME_DIR%\bin` or your Windows system folder.
    * In Linux, it generally in `/usr/lib/swi-prolog/lib/amd64/libjpl.so`
    * In MacOS, it can install in `/usr/local/lib/swipl-7.7.19/lib/x86_64-darwin17.7.0/libswipl.dylib`
 * `jpl.jar` must be found by any Java VMs (and compilers) used with JPL; one possibility is to put it on your global `CLASSPATH`.
 * `jpl.pl` is a Prolog source module, and must be found by any SWI-Prolog engines used with JPL
    * Would generally be in `%SWI_HOME_DIR%\library`
     

## MacOS

## Using Oracle's SDK

Download from

  - http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html

which installs

  - /Library/Java/JavaVirtualMachines/jdk1.7.0_45.jdk/Contents/Home

Set $JAVAPREFIX to

  - /Library/Java/JavaVirtualMachines/jdk1.7.0_45.jdk/Contents/Home/bin

 .dylib
 
 ## Windows
 
If the example programs don't run successfully, look carefully at any error messages; they typically indicate where the problem lies.  Then check the installation instruction s and prerequisites carefully. 
  
 * If you get a message about
 
        ... jpl.dll ... Access is denied ...
 
    then you may have lost execute permission on `jpl.dll` (please consult local Windows expertise if you don't know how to correct this).
 
 * If `jpl_examples/0` complains that
 
        The dynamic link library jvm.dll could not be found in the specified path 
  
    then you should locate jvm.dll within the Java runtime which you intend to use, and ensure that its directory is within the `PATH`.
 
 * If the Java examples (e.g. `jpl\examples\Exception\run.bat`) complain that 
  
        The name specified is not recognized as an internal or external command, operable program or batch file. 
   
     then there is no Java executable java.exe in any folder on your `PATH`: you should have a `PATH` entry such as `C:\jdk1.3.1_01\bin;` 
     
 * If the Java examples complain that
 
        The dynamic link library libpl.dll could not be found in the specified path
     or
     
        Exception in thread "main" java.lang.UnsatisfiedLinkError: C:\paul\bin\jpl.dll: Can't find dependent libraries 
 
     then there is no SWI-Prolog library `libpl.dll` in any folder on your `PATH`: you should have a `PATH` entry such as `C:\Program Files\pl\bin`
