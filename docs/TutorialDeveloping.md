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

The second thing to understand is that JPL is just a package on the overall SWI Prolog system. So, to compile the whole JPL, one needs to core SWI system as the JPL C component (`libswipl.so`) links against the core SWI library (e.g., `libswipl.so`):


So, first we get everything needed:

```bash
git clone git@github.com:SWI-Prolog/swipl-devel.git    // get main SWI distribution
cd swipl-devel/  
    
git submodule update --init // get sources for all modules
                                    
git submodule update --init packages/jpl        // get JPL source code
git submodule update --init packages/plunit     // get SWIPL unit testing
```

Next, make sure that any SWI Prolog system install that you have does not interfere with your development install. Make sure you do not have `LD_PRELOAD` or `LD_LIBRARY_PATH` pointing to your local install version of `libswipl.so` (e.g., `/usr/lib/libswipl.so`). Otherwise, your development will use that system install, which is not what you want:

```bash
unset LD_LIBRARY_PATH LD_PRELOAD SWI_HOME_DIR   # don't use SWI installs
```

Also, make sure the following packages are installed (to get nice history in SWIPL):

```bash
sudo apt-get install libreadline-dev libedit-devl 
```

Also, JPL will be compiled with SWIPL only if Java and JNI are found. The latter one is found when `JAVA_HOME` is set to the root of the Java to be used:

```bash
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64/
```

Now, **build the SWIPL system**  (SWIPL + JPL + PLUNIT) using CMAKE:

```bash
mkdir build
cd build

// for make
cmake -DINSTALL_DOCUMENTATION=OFF ..
make -j 8   

// for ninja
cmake -G Ninja -DINSTALL_DOCUMENTATION=OFF ..
ninja -v -j 8

ctest -V -R jpl:prolog_in_java  // run test of Java calling Prolog
ctest -V -R jpl:java_in_prolog  // run test of SWI calling Java
```

To **update/upgrade** the SWIPL framework to the latest one:

```bash
cd swipl-devel
git pull
git submodule update --init

unset LD_LIBRARY_PATH LD_PRELOAD SWI_HOME_DIR   # to make sure no use of old SWI install

cd build        # already exists form previous set-up
make -j8 // or ninja
sudo make install
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



## Developing & Testing JPL Java API from an IDE

Most probably one develops the Java-API of JPL from an IDE, for example IntelliJ or ECLIPSE and would like to run and test from the IDE itself, rather than from the CLI via CMAKE for the whole SWIPL framework (the C library `libjpl.so` most of often won't change).

To do so, we need to configure the IDE to make sure the correct resources under development, including native libraries `libswipl.so` and `libjpl.so`, are found and used. It may also be necessary to pre-load `libswipl.so` to avoid certain run-time errors with libraries that are not crossed-linked and specify the `jpl.jar` in the CLASSPATH for those Prolog tests that call Prolog.

In a nutshell, assuming we are develping JPL in its usual location with SWIPL (`packages/jpl`) and the SWIPL non-installed build is in `build/`, we would set-up the following environment variables:

```bash
SWI_HOME_DIR=../../build/home/
SWIPL_BOOT_FILE=../../build/home/boot.prc   # to setup SWIPL engine
LD_LIBRARY_PATH=../../build/packages/jpl    # to find libjpl.so
CLASSPATH=out/artifacts/jpl_jar/	        # for calls from Prolog to Java
LD_PRELOAD=../../build/src/libswipl.so	    # avoid run-time errors
TEST_DIR=src/java/org/jpl7/test 		    # find aux .pl testing support files
``` 

In IntelliJ, for example, this is done by modifying the [Java/JUnit Template]((https://intellij-support.jetbrains.com/hc/en-us/community/posts/205820189-How-to-set-default-environment-variables-) in "Run/Debug Configuration" to include the environment variables.

For instance, modify the JUnit template by just enter this string in "Environment variables":
        
```bash
LD_LIBRARY_PATH=../../build/packages/jpl;SWI_HOME_DIR=../../build/home;SWIPL_BOOT_FILE=../../build/home/boot.prc;CLASSPATH=out/artifacts/jpl_jar/;LD_PRELOAD=../../build/src/libswipl.so
```

Note this assumes you have produced `jpl.jar` as an artifact under `out/`.

Finally, you can create a new Run from the template, it will already inherit the environment variables. The whole JUNIT testing framework is aggregated by class `org.jpl7.test.JPLTestSuite`.


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
test 62
    Start 62: jpl:prolog_in_java

62: Test command: /usr/bin/env "CLASSPATH=src/java/jpl.jar" "SWI_HOME_DIR=../../home" "SWIPL_BOOT_FILE=../../home/boot.prc" "SOURCE_DIR=/home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl" "/usr/lib/jvm/java-11-openjdk-amd64/bin/java" "-Djava.library.path=." "-classpath" "/usr/share/java/junit4.jar:/usr/share/java/hamcrest-core.jar:src/java/jpl.jar:src/java/jpltest.jar" "org.jpl7.test.JPLTestSuiteRunner"
62: Test timeout computed to be: 9.99988e+06
62: Starting test: testAtomToString1 (Test_Atom)
62: Starting test: testAtomToString2 (Test_Atom)
62: Starting test: testAtomToString3 (Test_Atom)
62: Starting test: testAtomEquality1 (Test_Atom)
62: Starting test: testAtomEquality2 (Test_Atom)
62: Starting test: testAtomIdentity (Test_Atom)
62: Starting test: testAtom1 (Test_Atom)
62: Starting test: testAtomHasFunctorWrongName (Test_Atom)
62: Starting test: testAtomHasFunctorNameZero (Test_Atom)
62: Starting test: testAtomArity (Test_Atom)
62: Starting test: testAtomName1 (Test_Atom)
62: Starting test: testAtomName2 (Test_Atom)
62: Starting test: testAtomName3 (Test_Atom)
....

62: Starting test: test_atom_equal2 (Test_Equals)
62: Starting test: test_atom_equal3 (Test_Equals)
62: Starting test: test_atom_equal4 (Test_Equals)
62: Starting test: test_compound_hash1 (Test_Equals)
62: Starting test: test_atom_equals4 (Test_Equals)
62: ********* Test successful? true
1/1 Test #62: jpl:prolog_in_java ...............   Passed    0.63 sec

The following tests passed:
	jpl:prolog_in_java

100% tests passed, 0 tests failed out of 1

Total Test time (real) =   0.63 sec
```


## Using non-installed development tree of SWIPL & JPL in Java application

Suppose you are developing JPL as above, in this case at `/home/ssardina/git/soft/prolog/swipl-devel.git`

However, you have a Java application X  besides JPL itself and want to test that application under the current SWIPL+JPL development. There are several things that need to be set-up for your application to correctly access the SWIPL+JPL under development. 

First, we need to **setup various environment variables** so that the correct SWIPL+JPL is initialized and used:

	SWI_HOME_DIR=/home/ssardina/git/soft/prolog/swipl-devel.git/build/home
	SWIPL_BOOT_FILE=/home/ssardina/git/soft/prolog/swipl-devel.git/build/home/boot.prc
	LD_LIBRARY_PATH=/home/ssardina/git/soft/prolog/swipl-devel.git/build/packages/jpl
	LD_PRELOAD=/home/ssardina/git/soft/prolog/swipl-devel.git/build/src/libswipl.so
	CLASSPATH=/home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/out/artifacts/jpl_jar/
	TEST_DIR=/home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/src/java/org/jpl7/test 		    # find aux .pl testing support

The `CLASSPATH` needs to point to the `jpl.jar` file that needs to be us used in case `jpl.pl` requires it (to access JAVA from Prolog).

In one line (to copy into the usual Run-configuration of IDE):

	LD_LIBRARY_PATH=/home/ssardina/git/soft/prolog/swipl-devel.git/build/packages/jpl;SWI_HOME_DIR=/home/ssardina/git/soft/prolog/swipl-devel.git/build/home;SWIPL_BOOT_FILE=/home/ssardina/git/soft/prolog/swipl-devel.git/build/home/boot.prc;CLASSPATH=/home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/out/artifacts/jpl_jar/;LD_PRELOAD=/home/ssardina/git/soft/prolog/swipl-devel.git/build/src/libswipl.so;TEST_DIR=/home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/src/java/org/jpl7/test

Second, we need to tell our applicatoin X development to use the development JPL Java, and NOT any other `jpl.jar`  that may be in the system (e.g., due to a distribution installation). In our example, under IntelliJ, we define a library `jpl` in the project of the application pointing to directory:

	/home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/out/production/jpl

where all the classes for the currente devel JPL are located (if we are using IntelliJ). Then provded that library to use for your application/module for compilation and runtime.

Third, and finally, you need to explicitly initialize the SWIPL JPL engine in your Java application to with the right home, executable, and most importantly `-F swipl` so that the initialization file `swipl.rc` in charge of setting up all the search paths:

```java
public static final String swi_home = System.getenv("SWI_HOME_DIR"));
public static final String swi_startup  = System.getenv("SWIPL_BOOT_FILE"));
public static final String swi_exec = String.format("%s/../src/swipl", home);

String init_swi_config =
        String.format("%s -x %s -F swipl --home=%s -f none -g true -q",
            swi_exec, swi_startup, swi_home);
JPL.setDefaultInitArgs(init_swi_config.split("\\s+"));	# initialize SWIPL engine
JPL.init()
```

Nonetheless, because SWIPL tries to "guess" the location of the binary and boot file, in most cases, it also suffies to do:

```java
String init_swi_config =
	String.format("dummy --home=%s -g true -q",
		System.getenv("SWI_HOME_DIR"));
JPL.setDefaultInitArgs(init_swi_config.split("\\s+"));	# initialize SWIPL engine
```



