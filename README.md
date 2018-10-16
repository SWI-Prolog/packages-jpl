# JPL - Java <-> SWI Prolog Interface

JPL is a set of Java classes and C functions providing a **bidirectional interface between Java and Prolog**.  JPL uses the Java Native Interface (JNI) to connect to a Prolog engine through the Prolog Foreign Language Interface (FLI).  JPL is not a pure Java implementation of Prolog; it makes extensive use of native implementations of Prolog on supported platforms.  

In its current version, JPL supports the _embedding of a Prolog engine within the Java VM_ as well as the _embedding of a Java VM within Prolog_, so that, for example, one could take advantage of the rich class structure of the Java environment from within Prolog. 

JPL is designed in two layers, a low-level interface to the Prolog FLI and a high-level Java interface for the Java programmer who is not concerned with the details of the Prolog FLI.  The low-level interface is provided for C programmers who may wish to port their C implementations which use the FLI to Java with minimal fuss. The current version of JPL only works with [SWI-Prolog](http://www.swi-prolog.org/).

JPL has been integrated into the full [SWI-Prolog](http://www.swi-prolog.org/) distribution starting with version 5.4.x, including binaries for MS-Windows and a Linux RPM. 

For more information and specific documentation, refer to this [JPL Wiki](https://github.com/ssardina-research/packages-jpl/wiki).



