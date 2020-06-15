# Unit testing output

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


## Using the live devel tree

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



