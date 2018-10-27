# JPL

## Introduction

JPL is a set of Java classes and C functions providing a bidirectional interface between Java and Prolog.  JPL uses the Java Native Interface (JNI) to connect to a Prolog engine through the Prolog Foreign Language Interface (FLI).  JPL is not a pure Java implementation of Prolog; it makes extensive use of native implementations of Prolog on supported platforms.  The current version of JPL only works with SWI-Prolog.

In its current version, JPL supports the embedding of a Prolog engine within the Java VM as well as the embedding of a Java VM within Prolog, so that, for example, one could take advantage of the rich class structure of the Java environment from within Prolog.

JPL is designed in two layers, a low-level interface to the Prolog FLI and a high-level Java interface for the Java programmer who is not concerned with the details of the Prolog FLI.  The low-level interface is provided for C programmers who may wish to port their C implementations which use the FLI to Java with minimal fuss.

JPL has been integrated into the full SWI-Prolog distribution starting with version 5.4.x, including binaries for MS-Windows and a Linux RPM. Check an overview of its many versions [here](VERSIONS).

## Objectives

The objectives of JPL are to:

* enable Prolog applications to exploit any Java classes, instances, methods etc. (without requiring any wrappers, metadata etc. to be set up first);
* enable Java applications to manipulate any Standard Prolog libraries, predicates, etc. (without requiring any wrappers, metadata etc. to be set up first);
* enable hybrid Prolog+Java applications to be designed and implemented so as to take best advantage of both language systems, and to be testable, debuggable, maintainable.

.. while also aim for:

* minimum impact deployability: runtime support for Prolog+Java apps must be a position-independent, self-sufficient filestore tree, requiring no changes to registries, system libraries, system configuration files etc.
* minimum dependency deployability: as with JVMs, the Prolog+Java runtime support must depend upon nothing which cannot be taken for granted in healthy OS installations
* minimum vulnerability deployability: the Prolog+Java runtime support must be immune to legitimate variations in its environment (PATH settings, other applications and libraries including other Prolog+Java apps, etc.)



## Table of contents

1. Setup and Install
    * [Installing JPL in Linux and Windows](JPL-Under-Linux-and-Windows).
    * [Installing JPL in MacOS](JPL-under-Mac-OS): how to set it up under Mac OS.
    * [Developing JPL](DevelopingJPL): how to set-up a development install for JPL.
2. Guides and Tutorials:
    * [Getting Started: A Tutorial](GettingStartedAnExampleTutorial): a walkthrough how one would use JPL from Java.
3. To provide a dynamic, bidirectional, interface between SWI-Prolog and Java runtimes, JPL offers two APIs:
    * The [Java API](JavaAPI): this interface comprises public Java classes which support:
        * constructing Java representations of Prolog terms and queries;
        * calling queries within SWI-Prolog engines;
        * retrieving (as Java representations of Prolog terms) any bindings created by a call; 
    * The [Prolog API](PrologAPI): this interface comprises Prolog library predicates which support:
        * creating instances (objects) of Java classes (built-in and user-defined);
        * calling methods of Java objects (and static methods of classes), perhaps returning values or object references
        * getting and setting the values of fields of Java objects and classes.
4. Specific Information:
    * [Version History](VERSIONS): a short overview of the various versions of JPL since its creation.
    * [Types of Queries: One shot vs Iterative](TypesofQueries): reference of the main high-level interface offered to access Prolog from Java.
    * [Multi Threaded Queries](MultiThreadedQueries): how to use JPL with iterative queries and multi-threaded applications.
5. Applications: projects known where JPL has been used.



## Other resources

* The main Github repository for JPL 7 (which this one is a fork of): <https://github.com/SWI-Prolog/packages-jpl> 
* Documentation:
    * The main current JPL 7 site documentation: <https://jpl7.org/>
    * The documentation of SWI `library(jpl)`: <http://www.swi-prolog.org/pldoc/man?section=jpl>
    * A Wiki on JPL (including a Getting Started guide: <https://github.com/ssardina-research/packages-jpl/wiki>
    * The old JPL 3.x documentation: <http://www.swi-prolog.org/packages/jpl/>
* The [Java Native Interface](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/) programming framework.



## License

JPL is released under the terms of the Simplified BSD License. See [LICENSE](https://github.com/ssardina-research/packages-jpl/blob/master/LICENSE) file.
