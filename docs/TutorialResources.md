# Resources and Environment Variables

As explained in [Deployment](Deployment), there are several resources that need to be found to run embedded applications. Different environment variables will help the application to find those resources. For the examples below, we suppose we have compiled and locally install SWIPL+JPL under `/usr/local/swipl-git`.

First, the variable `SWI_HOME_DIR` specifies the root of the SWIPL install and helps, among other things, to find the `.pl` libraries (under library/`). It is not necessary if using the standard SWIPL install, but may be useful if using a differnet one, e.g.,:

```bash
export SWI_HOME_DIR=/usr/local/swipl-git/lib/swipl/
```

This variable is one of the ways that [SWIPL will try to find/guess the resources](https://www.swi-prolog.org/FAQ/FindResources.html). 


Also, if one is not using the default install (e.g., the one coming with Ubuntu), one needs to set-up `LD_LIBRARY_PATH` to find the Native `.so` libraries (including `libjpl.so`!):

```bash
LD_LIBRARY_PATH=$SWI_HOME_DIR/lib/x86_64-linux/ 
```

For `jpl.pl`, the Java application, or the IDE (e.g., IntelliJ or ECLIPSE)  to have access to the right Java API, the `CLASSPATH` has to include the appropiate `jpl.jar`: 

```bash
export CLASSPATH=$SWI_HOME_DIR/lib/jpl.jar:$CLASSPATH
```

Note also that one may need to tell your development environment or applications to use the correct `jpl.jar`, by correctly setting the `CLASSPATH` variable.

If you want to call Java from Prolog using JPL, you need to add the directory holding the JVM shared objects to the dynamic linker search path:
 
 ```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/jvm/java-11-openjdk-amd64/lib/server/
```

Also, some applications may require the pre-loading of libraries, e.g.,:

```bash
export LD_PRELOAD=$SWI_HOME_DIR/lib/x86_64-linux/libswipl.so
```


Finally, and in particular for development `JAVA_HOME` is needed to specify which Java is been used:


```bash
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64/
```

