# JPL architecture & components

The first thing to understand is that JPL has three parts:

1. C code (in `src/c`) implementing the bridge between Java and SWI Prolog (as installed in the specific architecture where execution will happen). This code uses the [Java Native Interface (JNI)](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/) programming framework, and is basically file `src/c/jpl.c`
    * Check [this](https://www.baeldung.com/jni) and [this](http://www.ntu.edu.sg/home/ehchua/programming/java/javanativeinterface.html) tutorials for understanding how JNI can be used. Also check IBM's [Best practices for using the Java Native Interface
](https://www.ibm.com/developerworks/library/j-jni/index.html)
2. Java code (in `src/java`) implementing the [Java-side API](https://jpl7.org/JavaApiOverview.jsp) interface of JPL to access Prolog. Basically this is a set of objects and methods to use SWI Prolog from Java. For example, objects that represent Prolog atom and terms, and methods to prove a goal and access the solution bindings. 
3. Prolog code (basically `jpl.pl`) implementing the [Prolog API](https://jpl7.org/PrologApiOverview.jsp) for access Java, for example to create an object or call a method of a given object. The reference documentation for the Prolog API can be found [here](http://www.swi-prolog.org/pldoc/doc/_SWI_/library/jpl.pl) too.
 
Both the Java and Prolog sources will make use of the C JNI code.
