# Unit testing output

Once you have the SWIPL compiled, it is time to test it. To do so, we use CMAKE `ctest` with two testing packs:
 * `jpl:java_in_prolog`, for testing Prolog-calls-Java; and 
 * `jpl:prolog_in_java` (for testing Java-calls-Prolog).

To run the tests with minimal output:

```bash
ssardina@Thinkpad-X1 build]$ ctest -R jpl:prolog_in_java
Test project /home/ssardina/git/soft/prolog/swipl-devel.git/build
    Start 63: jpl:prolog_in_java
1/1 Test #63: jpl:prolog_in_java ...............   Passed    0.79 sec

100% tests passed, 0 tests failed out of 1

Total Test time (real) =   0.82 sec
[ssardina@Thinkpad-X1 build]$ ctest -R jpl:java_in_prolog
Test project /home/ssardina/git/soft/prolog/swipl-devel.git/build
    Start 64: jpl:java_in_prolog
1/1 Test #64: jpl:java_in_prolog ...............   Passed    0.57 sec

100% tests passed, 0 tests failed out of 1

Total Test time (real) =   0.59 sec
```

One can use the verbose `-V` option as follows:

```bash
# ctest -V -R jpl:java_in_prolog
UpdateCTestConfiguration  from :/home/ssardina/git/soft/prolog/swipl-devel.git/build/DartConfiguration.tcl
UpdateCTestConfiguration  from :/home/ssardina/git/soft/prolog/swipl-devel.git/build/DartConfiguration.tcl
Test project /home/ssardina/git/soft/prolog/swipl-devel.git/build
Constructing a list of tests
Done constructing a list of tests
Updating test list for fixtures
Added 0 tests to meet fixture requirements
Checking test dependency graph...
Checking test dependency graph end
test 64
    Start 64: jpl:java_in_prolog

64: Test command: /home/ssardina/git/soft/prolog/swipl-devel.git/build/src/swipl "-p" "foreign=:/home/ssardina/git/soft/prolog/swipl-devel.git/build/packages/plunit" "-f" "none" "--no-packs" "-s" "/home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/test_jpl.pl" "-g" "test_jpl" "-t" "halt"
64: Test timeout computed to be: 9.99988e+06
64: % PL-Unit: jpl .......................................................................................... done
64: % 3 tests are blocked:
64: % /home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/test_jpl.pl:610:
64: 	test method_static_echo_float_4: we do not yet widen unbounded integers to floats or doubles
64: % /home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/test_jpl.pl:914:
64: 	test set_field_static_shadow_1: we do not yet resolve same-named shadowed fields
64: % /home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl/test_jpl.pl:1193:
64: 	test throw_java_exception_1: part of the error term is nondeterministic: we need to match with _
64: % 90 tests passed
1/1 Test #64: jpl:java_in_prolog ...............   Passed    0.58 sec

The following tests passed:
	jpl:java_in_prolog

100% tests passed, 0 tests failed out of 1

Total Test time (real) =   0.60 sec
```

Similarly, for the Java-calls-Prolog test:

```java
# ctest -V -R jpl:prolog_in_java
UpdateCTestConfiguration  from :/home/ssardina/git/soft/prolog/swipl-devel.git/build/DartConfiguration.tcl
UpdateCTestConfiguration  from :/home/ssardina/git/soft/prolog/swipl-devel.git/build/DartConfiguration.tcl
Test project /home/ssardina/git/soft/prolog/swipl-devel.git/build
Constructing a list of tests
Done constructing a list of tests
Updating test list for fixtures
Added 0 tests to meet fixture requirements
Checking test dependency graph...
Checking test dependency graph end
test 63
    Start 63: jpl:prolog_in_java

63: Test command: /usr/bin/env "CLASSPATH=src/main/java/jpl.jar" "SWI_HOME_DIR=../../home" "SWIPL_BOOT_FILE=../../home/boot.prc" "SOURCE_DIR=/home/ssardina/git/soft/prolog/swipl-devel.git/packages/jpl" "/usr/lib/jvm/java-11-openjdk-amd64/bin/java" "-Djava.library.path=." "-classpath" "/usr/share/java/junit4.jar:/usr/share/java/hamcrest-core.jar:src/main/java/jpl.jar:src/test/java/jpltest.jar" "org.jpl7.JPLTestSuiteRunner"
63: Test timeout computed to be: 9.99988e+06
63: Starting test: testAtomToString1 (Test_Atom)
63: Starting test: testAtomToString2 (Test_Atom)
63: Starting test: testAtomToString3 (Test_Atom)
63: Starting test: testAtomEquality1 (Test_Atom)
63: Starting test: testAtomEquality2 (Test_Atom)
63: Starting test: testAtomIdentity (Test_Atom)
63: Starting test: testAtom1 (Test_Atom)
...
...
...
63: Starting test: test_dict3 (Test_Dict)
63: Starting test: test_dict4 (Test_Dict)
63: Starting test: test_dict5 (Test_Dict)
63: ********* Test successful? true
1/1 Test #63: jpl:prolog_in_java ...............   Passed    1.06 sec

The following tests passed:
	jpl:prolog_in_java

100% tests passed, 0 tests failed out of 1

Total Test time (real) =   1.07 sec
```

