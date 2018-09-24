# JPL 7.4+ - Java - SWI Prolog Interface

## WHAT IS JPL?

JPL is a "library using the SWI-Prolog foreign interface and the Java jni interface providing a _bidirectional interface between Java and Prolog_ that can be used to embed Prolog in Java _as well_ as for embedding Java in Prolog. In both setups it provides a reentrant bidirectional interface."

As far as I can tell, JPL has been developed and maintained by Paul Singleton.

JPL has been integrated into the full SWI-Prolog distribution starting with version 5.4.x, including binaries for MS-Windows and a Linux RPM. 

There was an initial [JPL 3.x](http://www.swi-prolog.org/packages/jpl/), now discontinued, which then evolved into the current [JPL 7.4](https://jpl7.org/). 

The objectives of JPL are to:

* enable Prolog applications to exploit any Java classes, instances, methods etc. (without requiring any wrappers, metadata etc. to be set up first);
* enable Java applications to manipulate any Standard Prolog libraries, predicates, etc. (without requiring any wrappers, metadata etc. to be set up first);
* enable hybrid Prolog+Java applications to be designed and implemented so as to take best advantage of both language systems, and to be testable, debuggable, maintainable.

### Important Links

* The main current JPL 7 site: <https://jpl7.org/)
  * It includes the Java API (to access Prolog from Java) and the Prolog API (to access Java from Prolog).
* The main Github repository for JPL 7 (which this one is a fork of): <https://github.com/SWI-Prolog/packages-jpl> 
* The documentation of SWI `library(jpl)`: <http://www.swi-prolog.org/pldoc/man?section=jpl>
* The old JPL 3.x documentation: <http://www.swi-prolog.org/packages/jpl/>


## WHY THIS FORK THEN?

I have forked the main JPL Github repo mainly for two reasons:

1. Fix this issue [#9](<https://github.com/SWI-Prolog/packages-jpl/issues/9>) that preclude Java to perform queries on specific SWI modules.
  * This issue has been resoled and merged into the master branch of JPL in pull request [#10](https://github.com/SWI-Prolog/packages-jpl/pull/10) and hopefully will be available in SWI 7.7.X release.
2. Expose JPL repo as a Maven repo, so it can be obtained automatically as a dependency via [JitPack](https://jitpack.io/).

More generaly, I am using SWI+JPL to provide Prolog knowledgebase management to [SARL agent systems](http://www.sarl.io/). To do so, I have developed a SARL Capacity and Skill [SARL-PROLOG-CAP](https://bitbucket.org/ssardina-research/sarl-prolog-cap) that SARL agents can use to access SWI Prolog knowledgbases.  

One example of such use is the SARL Agent System to play the [2017 Multi-Agent Agents in City game](https://multiagentcontest.org/2017/). A base sytem, showcasing how to use SWI Prolog via JPL, can be obtained in [this repo](https://bitbucket.org/ssardina-research/sarl-agtcity-base). 


(notes from student The Champion in the context of my AOPD course in 2018)
 
There is an issue with using JPL in Mac OS due to a linking error in JPL. Here Theo describes his solution (edited with some extra info): 

The problem was that SWI-Prolog come as a _relocatable_ app and the JPL library is therefore _run-path dependent_, causing the linking error when loaded without the proper run-path search paths. That is JPL does not know where SWI is.
 
So, one way to solve this would to use a _not_ relocatable version of SWI-prolog such as the homebrew or macport installs. However, those do not include the JPL library. :-(
 
The only option is therefore to manually remove the run-path dependencies from the lib using the `install_name_tool`: 

    install_name_tool <old_path> <new_path> dynlib

So the first step is to install a not relocatable version of SWI-prolog:

    brew install swi-prolog

This will install prolog in `/usr/local/Cellar/swi-prolog`

As the homebrew version does _not_ come with the JPL library (an issue on the homebrew github as been opened about this), the second step is therefore to buid the library from source. Alternatively, you can download the already compiled  [libjpl.dylib]() and copy it in:

    /usr/local/Cellar/swi-prolog/7.6.4/libexec/lib/swipl-7.6.4/lib/

Now we need to ensure all dependencies link of the lib are valid (and absolute).
Then cd into this directory and check all the dependencies paths using the command:

    otool -L libjpl.dylib

For example:

    libjpl.dylib:
    /Applications/SWI-Prolog.app/Contents/swipl/lib/x86_64-darwin15.6.0/libjpl.dylib:
        @rpath/libjsig.dylib (compatibility version 1.0.0, current version 1.0.0)
        @rpath/libjvm.dylib (compatibility version 1.0.0, current version 1.0.0)
        @executable_path/../swipl/lib/x86_64-darwin15.6.0/libswipl.dylib (compatibility version 0.0.0, current version 7.6.4)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1226.10.1)


If an entry is not a valid path on your system you can change it using the command:

    install_name_tool -change <the_invalid_path> <your_new_valid_path> libjpl.dylib

Above, for example, the problem is with `libswipl.dylib`, which is not installed in  `@executable_path/../swipl/lib/x86_64-darwin15.6.0/libswipl.dylib` but instead in `/usr/local/lib/swipl-7.7.19/lib/x86_64-darwin17.7.0/libswipl.dylib`

After doing this step using `install_name_tool`, your lib should not depend on any external run-path:

    libjpl.dylib:
     /Library/Java/JavaVirtualMachines/jdk1.8.0_181.jdk/Contents/Home/jre/lib/server/libjsig.dylib (compatibility version 1.0.0, current version 1.0.0)
     /Library/Java/JavaVirtualMachines/jdk1.8.0_181.jdk/Contents/Home/jre/lib/server/libjvm.dylib (compatibility version 1.0.0, current version 1.0.0)
     /usr/local/lib/swipl-7.7.19/lib/x86_64-darwin17.7.0/libswipl.dylib (compatibility version 0.0.0, current version 7.7.19)
     /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1252.50.4)


Once all entries are valid, you can add the path to this library:

    /usr/local/Cellar/swi-prolog/7.6.4/libexec/lib/swipl-7.6.4/lib/ 

to your `java.path.libray` and it will, hopefully, link correctly: :-)
  
    java -Djava.library.path=/usr/local/Cellar/swi-prolog/7.6.4/libexec/lib/swipl-7.6.4/lib/x86_64-darwin17.3.0/
 
The Java Virtual Machine (JVM) uses the `java.library.path` property in order to locate and load _native libraries_. 
When a Java application loads a native library using the `System.loadLibrary()` method, the `java.library.path` is scanned for the specified library. 
If the JVM is not able to detect the requested library, it throws an `UnsatisfiedLinkError`. 
See [this tutorial](https://examples.javacodegeeks.com/java-basics/java-library-path-what-is-it-and-how-to-use/) for more explanation on that variable.


You can also do this in Eclipse: 

1. Select your project in the Package Explorer area and press a right click on it.
2. Select `Build Path → Configure Build Path...` option.
3. In the appearing window, select the `Libraries` tab.
4. Then, expand the JRE System library option and select the Native library location.
5. Click on the Edit... button at the right panel.
6. Locate the required library and then click OK.
 
And that's it, your project will link and run properly on Mac OS ;)

Thanks Theo!

## CONTACT

Sebastian Sardina - ssardina@gmail.com




