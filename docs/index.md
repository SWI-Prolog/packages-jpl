# Introduction

JPL is a set of Java classes and C functions providing a bidirectional interface between Java and Prolog.  JPL uses the Java Native Interface (JNI) to connect to a Prolog engine through the Prolog Foreign Language Interface (FLI).  JPL is not a pure Java implementation of Prolog; it makes extensive use of native implementations of Prolog on supported platforms.  The current version of JPL only works with SWI-Prolog.

In its current version, JPL supports the embedding of a Prolog engine within the Java VM as well as the embedding of a Java VM within Prolog, so that, for example, one could take advantage of the rich class structure of the Java environment from within Prolog.

JPL is designed in two layers, a low-level interface to the Prolog FLI and a high-level Java interface for the Java programmer who is not concerned with the details of the Prolog FLI.  The low-level interface is provided for C programmers who may wish to port their C implementations which use the FLI to Java with minimal fuss.

JPL has been integrated into the full SWI-Prolog distribution starting with version 5.4.x, including binaries for MS-Windows and a Linux RPM. Check a high-level overview of its many versions [here](ReleaseNotes.md).

For the JPL source code please refer to its GitHub repository [here].

JPL is developed in the GitHub [packges-jpl](https://github.com/SWI-Prolog/packages-jpl) repository withn the [SWI-Prolog](https://github.com/SWI-Prolog) organization. 

## Objectives

The _objectives_ of JPL are to:

* enable Prolog applications to exploit any Java classes, instances, methods etc. (without requiring any wrappers, metadata etc. to be set up first);
* enable Java applications to manipulate any Standard Prolog libraries, predicates, etc. (without requiring any wrappers, metadata etc. to be set up first); and
* enable hybrid Prolog+Java applications to be designed and implemented so as to take best advantage of both language systems, and to be testable, debuggable, maintainable.

... while also aiming for:

* minimum impact deployability: runtime support for Prolog+Java apps must be a position-independent, self-sufficient filestore tree, requiring no changes to registries, system libraries, system configuration files etc;
* minimum dependency deployability: as with JVMs, the Prolog+Java runtime support must depend upon nothing which cannot be taken for granted in healthy OS installations; and
* minimum vulnerability deployability: the Prolog+Java runtime support must be immune to legitimate variations in its environment (PATH settings, other applications and libraries including other Prolog+Java apps, etc.).

### Historical perspective

JPL was originally dreamed to be independent of Prolog implementation, mapping only the classic Prolog term model to and from Java, and its Java-side implementation deliberately resisted adoption of post Java 1.4 features (some nice, but none irresistible), thereby maintaining compatibility with (now very) old JVMs which some deployment environments might be stuck with.

Innovations in SWI Prolog 7 prompted a JPL overhaul (from 3.0.3 to 7.0.1) which changed the public API and warranted a rebrand (as "JPL7", in a nod to SWIPL7).

(Summary taken from discussion in [this issue](https://github.com/SWI-Prolog/packages-jpl/issues/46).)


## About this page

This page includes _documentation_ on how to install and setup JPL, the APIs offered, guides and tutorials, and version release notes.

If you are only wanting to *use* JPL in your application (mostly in your Java application), you may then need to read how to **deploy JPL** in your system in oder to be able to use it. Check the guides for deployment on the various platforms: [Linux](DeploymentLinux.md), [Windows](DeploymentWindows.md) or [MacOS](DeploymentMacos.md). 

To provide a dynamic, bidirectional, interface between SWI-Prolog and Java runtimes, **JPL offers two APIs**: a [Java API](JavaApi.md) to access Prolog from Java and a [Prolog API](PrologApi.md). Corresponding JavaDoc and Reference manual are also provided for both APIs.

There are also some useful **tutorials** for those wanting to use JPL, including a [getting started](TutorialGettingStarted.md) guide, a discussion on the [two types of Prolog queries](TutorialTypesOfQueries.md) offered from Java and the subletites to consider when your application can potentially issue [multi-threaded queries](TutorialMultithreaded.md) (i.e., multiple queries at the same time in different threads).


If you want to **develop JPL** further or want to implement some modification, you will need to carry an full installation from scratch, including an install/compile of SWI source. Please refer to the [Developing JPL](TutorialDeveloping) guide.


This page also contains a short overview of the various [release versions](ReleaseNotes) of JPL as well as release notes for the main releases.



## Applications

There are many projects out there using SWI+JPL. If you have one project using JPL, and you are happy for it to be listed here, please let us know and we will include it. This will help others understand the potential of embedding SWI in their systems.

One example is the [SARL-PROLOG-CAP](https://github.com/ssardina-agts/sarl-prolog-cap) project, which provides a capacity/skill  for [SARL](http://sarl.io) agents to access SWI Prolog knowledgebases. It is then used in  the [SARL Elevator Controller](https://github.com/ssardina-agts/elevator-sarl-base) and [SARL Agents in City](https://github.com/ssardina-agts/agtcity-sarl-base) systems.
    



## Other resources

* The main Github repository for JPL 7 (which this one is a fork of): <https://github.com/SWI-Prolog/packages-jpl> 
* Documentation:
    * The main current JPL 7 site documentation (this page!): <https://jpl7.org/>
    * The documentation of SWI `library(jpl)`: <http://www.swi-prolog.org/pldoc/man?section=jpl>
    * A Wiki on JPL (hopefully now subsumed by this doc!): <https://github.com/ssardina-research/packages-jpl/wiki>
    * The old JPL 3.x documentation: <http://www.swi-prolog.org/packages/jpl/>
* The [Java Native Interface](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/) programming framework.


## License

JPL is released under the terms of the Simplified BSD License. See [LICENSE](https://github.com/SWI-Prolog/packages-jpl/blob/master/LICENSE) file.
