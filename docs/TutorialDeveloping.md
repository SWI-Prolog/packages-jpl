# Tutorials - Developing JPL

Say you want to develop JPL further, to extend it or address some bug.

## JPL architecture & components

The first thing to understand is that JPL has three parts:

1. C code (in `src/c`) implementing the bridge between Java and SWI Prolog (as installed in the specific architecture where execution will happen). This code uses the [Java Native Interface (JNI)](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/) programming framework, and is basically file `src/c/jpl.c`
    * Check [this](https://www.baeldung.com/jni) and [this](http://www.ntu.edu.sg/home/ehchua/programming/java/javanativeinterface.html) tutorials for understanding how JNI can be used. Also check IBM's [Best practices for using the Java Native Interface
](https://www.ibm.com/developerworks/library/j-jni/index.html)
2. Java code (in `src/java`) implementing the [Java-side API](https://jpl7.org/JavaApiOverview.jsp) interface of JPL to access Prolog. Basically this is a set of objects and methods to use SWI Prolog from Java. For example, objects that represent Prolog atom and terms, and methods to prove a goal and access the solution bindings. 
3. Prolog code (basically `jpl.pl`) implementing the [Prolog API](https://jpl7.org/PrologApiOverview.jsp) for access Java, for example to create an object or call a method of a given object. The reference documentation for the Prolog API can be found [here](http://www.swi-prolog.org/pldoc/doc/_SWI_/library/jpl.pl) too.
 
Both the Java and Prolog sources will make use of the C JNI code.


## Setting up SWI-Prolog for JPL for development

The second thing to understand is that JPL is just a package on the overall SWI Prolog system. So, to compile the whole JPL, one needs to core SWI system as the JPL C component (`libswipl.so`) links against the core SWI library (e.g., `libswipl.so`).


So, first we get everything needed:

```bash
git clone git@github.com:SWI-Prolog/swipl-devel.git    // get main SWI distribution
cd swipl-devel/                                      
git submodule update --init packages/jpl               // get JPL package source code
git submodule update --init packages/plunit            // get unit testing support SWI library 
```

Next, make sure that any SWI Prolog system install that you have does not interfere with your development install. Make sure you do not `LD_PRELOAD` or `LD_LIBRARY_PATH` pointing to your local install version of `libswipl.so` (e.g., `/usr/lib/swi-prolog/lib/amd64/libjpl.so`). Otherwise your development will use that system install, which is not what you want. 

Now, build the system obtained (SWI+JPL+PLUNIT) using CMAKE:

```bash
mkdir build
cd build
cmake -DINSTALL_DOCUMENTATION=OFF ..
make
ctest -V -R jpl:prolog_in_java  // run test of Java calling Prolog
ctest -V -R jpl:java_in_prolog  // run test of SWI calling Java
```

## Developing JPL

At this point you have SWI + JPL for development. However, you cannot commit changes to neither repositories. You need to do the changes in your own fork of JPL and once happy probably propose a pull request for the SWI+JPL developers to adopt (by merging your code into the main JPL repo).

So, as an example, I have my own fork of JPL at <https://github.com/ssardina-research/packages-jpl> and in that repo I have, say, a branch called `proposal` where I will do changes to later issue as pull requests to the main [JPL repo](https://github.com/SWI-Prolog/packages-jpl).

Because packages in the SWI distribution are independent git submodules, we can "attach" our JPL fork in the whole development set-up we build above:

```bash
<we assume we are at the main SWI local git repo, that is, swipl-devel/
cd packages/jpl/
git remote add ssardina git@github.com:ssardina-research/packages-jpl.git
git fetch --all      // will fetch all the info of branches in the new remote ssardina
git branch --track proposal ssardina/proposal // local branch proposal will start tracking ssardina/proposal
<do all your changes>
git push ssardina // assuming you are happy with the changes
```

Now we build:

```bash
cd ../../build
cmake -DINSTALL_DOCUMENTATION=OFF ..
make -C packages/jpl clean // clean the JPL previous compile
make
ctest -V -R jpl  // run tests to check nothing broke
```

Of course, the first time you will probably not have your `proposal` branch in your fork. So you can create one as follows:


```bash
<we assume we are at the main SWI local git repo, that is, swipl-devel/
cd packages/jpl/
git remote add ssardina git@github.com:ssardina-research/packages-jpl.git
git checkout -b proposal
<do all your changes>
git push -u ssardina proposal  // push new branch to remote ssardina and start tracking
```

Now you have a local branch `proposal` tracking a remote branch in `ssardina` for it.


**NOTE:** If you are _only_ expecting to change the Java API, then you can just work with the `packages-jpl` distribution in isolation. This will compile just the Java classes and produce a corresponding JAR file for use. However, in this case you would have to have an SWI system installed system-wide and make sure your application uses your updated JPL JAR file and not the system installed one.

## Set-up variables for developing JPL

When you develop JPL (e.g., change the Java API) you need to make sure your development set-up is using the correct SWI Prolog system, so that the right `libswipl.so` and `libjpl.so` are used when Java is calling SWI. For example, if you are using IntelliJ to develop the Java component of JPL.

Basically, one needs to set-up the environment variables `SWI_HOME_DIR` and `LD_LIBRARY_PATH` to wherever the SWI-Prolog to be used is located, as well as `LD_PRELOAD`:

    LD_LIBRARY_PATH=/usr/local/swipl-git/lib/swipl/lib/x86_64-linux/
    SWI_HOME_DIR=/usr/local/swipl-git/lib/swipl/
    LD_PRELOAD=libswipl.so

In IntelliJ, for example, this is done by modifying the Template in "Run/Debug Configuration" to include the environment variables needed ( `LD_PRELOAD`, `SWI_HOME_DIR`, and `LD_LIBRARY_PATH`). See this [post](https://intellij-support.jetbrains.com/hc/en-us/community/posts/205820189-How-to-set-default-environment-variables-)

For instance, modify the JUnit template by just enter this string in "Environment variables":
        
            LD_LIBRARY_PATH=/usr/local/swipl-git/lib/swipl/lib/x86_64-linux/;SWI_HOME_DIR=/usr/local/swipl-git/lib/swipl/;LD_PRELOAD=libswipl.so

Finally, you can create a new Run from the template, it will already inherit the environment variables.
* The main test class is `org.jpl7.test.TestJUnit` which is a bag of tests.
* There are also standalone tests in `org.jpl7.test.standalone.*`


## Unit testing output

This is how it looks when the tests are run error-free:

```bash
[ssardina@Thinkpad-X1 build]$ ctest -V -R jpl:prolog_in_java
UpdateCTestConfiguration  from :/home/ssardina/git/soft/prolog/swipl-devel.git/build/DartConfiguration.tcl
UpdateCTestConfiguration  from :/home/ssardina/git/soft/prolog/swipl-devel.git/build/DartConfiguration.tcl
Test project /home/ssardina/git/soft/prolog/swipl-devel.git/build
Constructing a list of tests
Done constructing a list of tests
Updating test list for fixtures
Added 0 tests to meet fixture requirements
Checking test dependency graph...
Checking test dependency graph end
test 13
    Start 13: jpl:prolog_in_java

13: Test command: /usr/bin/env "SWI_HOME_DIR=../../home" "TEST_JPL=../../../packages/jpl/test_jpl.pl" "/usr/lib/jvm/java-8-oracle/bin/java" "-Djava.library.path=." "-classpath" "/usr/share/java/junit.jar:src/java/jpl.jar:src/java/jpltest.jar" "junit.textui.TestRunner" "org.jpl7.test.TestJUnit"
13: Test timeout computed to be: 9.99988e+06
13: .........................................
13: .........................................
13: ......................................
13: Time: 0.06
13: 
13: OK (120 tests)
13: 
1/1 Test #13: jpl:prolog_in_java ...............   Passed    0.35 sec

The following tests passed:
	jpl:prolog_in_java

100% tests passed, 0 tests failed out of 1

Total Test time (real) =   0.36 sec
[ssardina@Thinkpad-X1 build]$ ctest -V -R jpl:java_in_prolog
UpdateCTestConfiguration  from :/home/ssardina/git/soft/prolog/swipl-devel.git/build/DartConfiguration.tcl
UpdateCTestConfiguration  from :/home/ssardina/git/soft/prolog/swipl-devel.git/build/DartConfiguration.tcl
Test project /home/ssardina/git/soft/prolog/swipl-devel.git/build
Constructing a list of tests
Done constructing a list of tests
Updating test list for fixtures
Added 0 tests to meet fixture requirements
Checking test dependency graph...
Checking test dependency graph end
test 12
    Start 12: jpl:java_in_prolog

12: Test command: /home/ssardina/git/soft/prolog/swipl-devel.git/build/src/swipl "-p" "foreign=:/home/ssardina/git/soft/prolog/swipl-devel.git/build/packages/plunit" "-p" "library=:/home/ssardina/git/soft/prolog/swipl-devel.git/packages/plunit" "-f" "none" "-s" "/home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/test_jpl.pl" "-g" "test_jpl" "-t" "halt"
12: Test timeout computed to be: 9.99988e+06
12: % PL-Unit: jpl .......................................................................................... done
12: % 3 tests are blocked:
12: % /home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/test_jpl.pl:610:
12: 	test method_static_echo_float_4: we do not yet widen unbounded integers to floats or doubles
12: % /home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/test_jpl.pl:914:
12: 	test set_field_static_shadow_1: we do not yet resolve same-named shadowed fields
12: % /home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/test_jpl.pl:1193:
12: 	test throw_java_exception_1: part of the error term is nondeterministic: we need to match with _
12: % 90 tests passed
1/1 Test #12: jpl:java_in_prolog ...............   Passed    0.35 sec

The following tests passed:
	jpl:java_in_prolog

100% tests passed, 0 tests failed out of 1

Total Test time (real) =   0.35 sec
[ssardina@Thinkpad-X1 build]$ 
```


## Using non-installed development tree of SWIPL & JPL in Java application

Suppose you are developing JPL as above, in this case at `/home/ssardina/git/soft/prolog/swipl-devel.git`

However, you have a Java application X  besides JPL itself and want to test that application under the current SWIPL+JPL development. There are several things that need to be set-up for your application to correctly access the SWIPL+JPL under development. 

First, we need to **setup various environment variables** so that the correct SWIPL+JPL is initialized and used:

	SWI_HOME_DIR=/home/ssardina/git/soft/prolog/swipl-devel.git/build/home
	SWI_EXEC_FILE=/home/ssardina/git/soft/prolog/swipl-devel.git/build/src/swipl
	SWIPL_BOOT_FILE=/home/ssardina/git/soft/prolog/swipl-devel.git/build/home/boot.prc
	LD_LIBRARY_PATH=/home/ssardina/git/soft/prolog/swipl-devel.git/build/packages/jpl
	LD_PRELOAD=/home/ssardina/git/soft/prolog/swipl-devel.git/build/src/libswipl.so
	CLASSPATH=/home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/out/artifacts/jpl_jar/

The `CLASSPATH` needs to point to the `jpl.jar` file that needs to be us used in case `jpl.pl` requires it (to access JAVA from Prolog).

In one line (to copy into the usual Run-configuration of IDE):

	LD_LIBRARY_PATH=/home/ssardina/git/soft/prolog/swipl-devel.git/build/packages/jpl;SWI_HOME_DIR=/home/ssardina/git/soft/prolog/swipl-devel.git/build/home;SWIPL_BOOT_FILE=/home/ssardina/git/soft/prolog/swipl-devel.git/build/home/boot.prc;CLASSPATH=/home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/out/artifacts/jpl_jar/;LD_PRELOAD=/home/ssardina/git/soft/prolog/swipl-devel.git/build/src/libswipl.so;SWI_EXEC_FILE=/home/ssardina/git/soft/prolog/swipl-devel.git/build/src/swipl


Second, we need to tell our applicatoin X development to use the development JPL Java, and NOT any other `jpl.jar`  that may be in the system (e.g., due to a distribution installation). In our example, under IntelliJ, we define a library `jpl` in the project of the application pointing to directory:

	/home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/out/production/jpl

where all the classes for the currente devel JPL are located (if we are using IntelliJ). Then provded that library to use for your application/module for compilation and runtime.

Third, and finally, you need to explicitly initialize the SWIPL JPL engine in your Java application to with the right home, executable, and most importantly `-F swipl` so that the initialization file `swipl.rc` in charge of setting up all the search paths:

```java
String init_swi_config =
	String.format("%s -x %s -F swipl --home=%s -g true -q",
		System.getenv("SWI_EXEC_FILE"), 	# irrelevant for Windows
		System.getenv("SWIPL_BOOT_FILE"), 	
		System.getenv("SWI_HOME_DIR"));
JPL.setDefaultInitArgs(init_swi_config.split("\\s+"));	# initialize SWIPL engine
JPL.init()
```

Because SWIPL tries to "guess" the location of the binary and boot file, in most cases, it also suffies to do:

```java
String init_swi_config =
	String.format("dummy --home=%s -g true -q",
		System.getenv("SWI_HOME_DIR"));
JPL.setDefaultInitArgs(init_swi_config.split("\\s+"));	# initialize SWIPL engine
```



