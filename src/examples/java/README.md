# JPL Examples for Java-calls-Prolog

This directory contains various example tests for JPL's Java-calls-Prolog interface.

Each example is within a package and has been tested with JPL 7.6.0 and SWIPL 8.2.0 (June 2020).

## Compiling and running the examples

To compile all examples in Linux:

```bash
find -name "*.java" > sources.txt
javac -cp /usr/lib/swi-prolog/lib/jpl.jar @sources.txt
```

In Windows:

```:: Windows
> dir /s /B *.java > sources.txt
> javac /usr/lib/swi-prolog/lib/jpl.jar @sources.txt
```

In one line: 

```bash
find . -name "*.java" -print | xargs javac -cp /usr/lib/swi-prolog/lib/jpl.jar
```

Tu **run the examples** we need to:
 
 * provide the location of `jpl.jar`, always. 
 * run it within its folder so it can find any corresponding resource `.pl` (Prolog source) files, if any.

For example, to run a particular example `X` that does _not_ require any resources (e.g., consulting a Prolog file) we can simply do:

```bash
java -cp .:/usr/lib/swi-prolog/lib/jpl.jar  x.X
```

If the example requires some resource, such as loading a Prolog `.pl` file, we need to cd into the location of the example and run it from there so that the resource is in the current user dir. For example:

```bash
cd <swi-home>/doc/packages/examples/jpl/java/family
java -cp ..:/usr/lib/swi-prolog/lib/jpl.jar family.Family
```


## Set of Examples


### Versions

The `system.Versions` class example tries to discover which versions (e.g. `7.6.0-alpha`) of each JPL library (i.e. Prolog, C and Java) are installed, and congratulates you if they are the same. It also reports the home and version of the SWIPL engine being used.

```shell script
[ssardina@Thinkpad-X1 system]$ java -cp ..:/usr/lib/swi-prolog/lib/jpl.jar system.Versions
	 swipl.home = /usr/lib/swi-prolog
	 swipl.version = 8.2.0
prolog library version; 7.6.0-alpha
  java library version; 7.6.0-alpha
     c library version; 7.6.0-alpha
BINGO! you appear to have the same version of each library installed
```

### Zahed
	
Creates a trivial-in-Prolog-but-horrible-in-JPL query goal and calls it against a trivial Prolog database (file `zahed.pl`).

The query term is built "from first principles", by composing the various data types one by one. Of course, in the new JPL, one uses recent facilities for creating query terms from Prolog source text fragments (namely, `Term.textToTerm()`).

```shell script
[ssardina@Thinkpad-X1 zahed]$ java -cp ..:/usr/lib/swi-prolog/lib/jpl.jar zahed.Zahed
starting...
The query constructed is: gen([t(c, q, []), t(v, [], a)], A)
ok
finished
```

### Test
	
Runs a simple set of tests of the High-Level Interface. You should run this test once you have gotten JPL built, or if you are experiencing any problems with the package.

The test loads a `test.pl` Prolog file.

```shell script
[ssardina@Thinkpad-X1 test]$ java -cp ..:/usr/lib/swi-prolog/lib/jpl.jar test.Test
test 0...passed
test 1...passed
test 2...passed
test 3...passed
test 4...passed
test 5...passed
test 6...passed
test 7...passed
test 8...passed
test 9...passed
test 10...passed
test 11...passed
test 101...00333121253535555555555555555554044324241042720202767460602161684747884644444691919199209126413113838738371114444629777077777777779898131898688882222222200000000008888886999999999911133333333666666666passed
```

### Db

The example `db.Db` will load a library, assert a new fact, and do the listing of the database.

```shell script
[ssardina@Thinkpad-X1 db]$ java -cp ../:/usr/lib/swi-prolog/lib/jpl.jar db.Db
	 Solution found (now asserting to block?): {X=john, Y=20, Z=melbourne}
	 Solution found (now asserting to block?): {X=maria, Y=31, Z=city(sydney)}
	 Solution found (now asserting to block?): {X=maria, Y=11, Z=city(perth)}
	 Solution found (now asserting to block?): {X=adam, Y=18, Z=geelong}
	 Solution found (now asserting to block?): {X=michelle, Y=14, Z=lorne}
:- dynamic name_of_person/1.

name_of_person(john).
name_of_person(maria).
name_of_person(maria).
name_of_person(adam).
name_of_person(michelle).
```


### Family

There are two variants in package `family`

The class `Family` consults some parent-child relationship clauses (from `family.pl`) and runs some simple queries.

```shell script
[ssardina@Thinkpad-X1 family]$ java -cp ../:/usr/lib/swi-prolog/lib/jpl.jar family.Family
swipl.version = 82.0
swipl.syntax = modern
swipl.home = /usr/lib/swi-prolog
jpl.jar = 7.6.0-alpha
jpl.dll = 7.6.0-alpha
jpl.pl = 7.6.0-alpha
consult('family.pl') succeeded
child_of(joe, ralf) is provable
descendent_of(steve, ralf) is provable
first solution of descendent_of(X, ralf): X = joe
all solutions of descendent_of(X, ralf)
X = joe
X = mary
X = steve
each solution of descendent_of(X, ralf)
X = joe
X = mary
X = steve
each solution of descendent_of(X,Y)
X = joe, Y = ralf
X = mary, Y = joe
X = steve, Y = joe
X = mary, Y = ralf
X = steve, Y = ralf
```


In turn, class `FamilyMT` consults the same parent-child relationship clauses (again, from `family.pl`) and then sets off twenty threads, each doing the
same queries of `Family`.  As there are only 10 available Prolog engines by default, the threads sometimes get to wait.

```shell script
[ssardina@Thinkpad-X1 family]$ java -cp ../:/usr/lib/swi-prolog/lib/jpl.jar family.FamilyMT
consult succeeded
spawning client[0]
spawning client[1]
spawning client[2]
child_of(joe,ralf) is provable
child_of(joe,ralf) is provable
spawning client[3]
....
```

### Factorial

This example is in `factorial` package and consults file `fact.pl`.

The example recursively computes `factorial(10)` by alternately calling Prolog from Java and Java from Prolog.

```bash
[ssardina@Thinkpad-X1 factorial]$ java -cp ..:/usr/lib/swi-prolog/lib/jpl.jar factorial.Factorial
calling Prolog to call Java to call Prolog...
factorial(10) = 3628800
```


### SemWeb

This demo is a very simple example  of loading and using the SWI-Prolog semantic web library for parsing and  querying   of  RDF  data. Its main purpose is actually to illustrate and test access from Prolog to foreign resources. This demo uses `sgml2pl.so` and `rdf_db.so`,  providing the XML parser and RDF database.

The library `libjpl.so` contains the Prolog kernel and therefore the extensions must get the `PL_*` symbols from this library. Java loads shared objects without making their symbols  available to other modules. This problem is avoided by _preloading_  `libjpl.so`.

```shell script
export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/jni/libjpl.so
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/jvm/java-11-openjdk-amd64/lib/server/
```

For example (check how the error on `libjvm.so` is resolved):

```shell script
[ssardina@Thinkpad-X1 semWeb]$ export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/jni/libjpl.so

[ssardina@Thinkpad-X1 semWeb]$ java -cp ..:/usr/lib/swi-prolog/lib/jpl.jar semWeb.SemWeb
java: error while loading shared libraries: libjvm.so: cannot open shared object file: No such file or directory

[ssardina@Thinkpad-X1 semWeb]$ export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/jvm/java-11-openjdk-amd64/lib/server/
[ssardina@Thinkpad-X1 semWeb]$ java -cp ..:/usr/lib/swi-prolog/lib/jpl.jar semWeb.SemWeb
use_module(library('semweb/rdf_db')) succeeded
% Parsed "test.rdf" in 0.06 sec; 3 triples
loaded
{'_:file:///home/ssardina/git/soft/prolog/swipl-devel-maven.git/packages/jpl/src/examples/java/org/jpl7/examples/semWeb/test.rdf#_:Description1', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#value', literal('200')}
{'_:file:///home/ssardina/git/soft/prolog/swipl-devel-maven.git/packages/jpl/src/examples/java/org/jpl7/examples/semWeb/test.rdf#_:Description1', 'http://www.nist.gov/units/units', 'http://www.nist.gov/units/Pounds'}
{'file:///home/ssardina/git/soft/prolog/swipl-devel-maven.git/packages/jpl/src/examples/java/org/jpl7/examples/semWeb/John_Smith', 'http://www.nist.gov/units/weight', '_:file:///home/ssardina/git/soft/prolog/swipl-devel-maven.git/packages/jpl/src/examples/java/org/jpl7/examples/semWeb/test.rdf#_:Description1'}
```


### Exceptions

This example is in `system` package and dit demonstrates how an uncaught Prolog exception is turned into a Java exception, which in this example is caught in Java.

```shell script
[ssardina@Thinkpad-X1 java]$ java -cp .:/usr/lib/swi-prolog/lib/jpl.jar  system.Exceptions
calling

?- X is Y.

in Prolog to force a Prolog 'instantiation_error' exception,
which should be returned via Java as an uncaught org.jpl7.PrologException in thread "main".

BINGO! The following exception was thrown: 
	PrologException: error(instantiation_error, context(':'(system, '/'(is, 2)), _2))
```


### TestGC

This example tests the garbage collector and deals with [Issue #4](https://github.com/SWI-Prolog/packages-jpl/issues/4): "_Memory leak   with
JPL (atoms are never GC'd by SWI)_".

```shell script
[ssardina@Thinkpad-X1 testGC]$ java -cp ..:/usr/lib/swi-prolog/lib/jpl.jar testGC.TestGC
10 iterations
% Started at Sun Jun 14 13:33:57 2020
% 0.241 seconds cpu time for 92,931 inferences
% 5,440 atoms, 3,928 functors, 2,722 predicates, 34 modules, 89,442 VM-codes
% 
%                     Limit   Allocated      In use
% Local  stack:           -       20 Kb    1,312  b
% Global stack:           -       64 Kb       28 Kb
% Trail  stack:           -       34 Kb       80  b
%        Total:    1,024 Mb      118 Kb       30 Kb
% 
% 1 atom garbage collections gained 235 atoms in 0.000 seconds.
```



### Time
	
Runs a set of timing experiments, designed to gauge the speed with which terms are exchanged between Java and Prolog.

```shell script
[ssardina@Thinkpad-X1 time]$ java -cp ..:/usr/lib/swi-prolog/lib/jpl.jar time.Time
test 0...passed
treesdone
traversedone
noopdone
noop_nobinddone
noop_binddone

num_trials: 10
tree_depth: 10 (= 1025 terms)

test_0		test_1		test_2		test_3		test_4		
0		5		2		2		2		
0		0		1		1		2		
0		0		1		1		3		
0		0		1		1		2		
1		0		1		1		2		
0		0		0		1		1		
0		0		1		1		2		
0		0		1		1		2		
0		0		2		1		2		
0		0		1		1		3		

test_0: avg: 0ms		0ms/term
test_1: avg: 0ms		0ms/term
test_2: avg: 1ms		0.001ms/term
test_3: avg: 1ms		0.001ms/term
test_4: avg: 2ms		0.002ms/term
```

## Init

This example is in `system` package and showcases how to [initialize JPL](https://jpl7.org/TutorialGettingStarted) with different [CLI options](https://www.swi-prolog.org/pldoc/man?section=cmdline).

Class `system.Init` can be initialized with any combination (including none) of `SWI_HOME_DIR` (for `--home` option), `SWIPL_BOOT_FILE` (for `-x` option), and `SWI_EXEC_FILE`.

For example, to include them all for the local SWIPL install located at `/usr/local/swipl-git/lib/swipl/`:

```shell script
LD_LIBRARY_PATH=/usr/local/swipl-git/lib/swipl/lib/x86_64-linux/ \
  SWI_HOME_DIR=/usr/local/swipl-git/lib/swipl/ \
  SWI_EXEC_FILE=/usr/local/swipl-git/lib/swipl/bin/x86_64-linux/swipl 
  SWIPL_BOOT_FILE=/usr/local/swipl-git/lib/swipl/boot.prc \
      java -cp ../:/usr/local/swipl-git/lib/swipl/lib/jpl.jar system.Init
```

Note that we need to set up `LD_LIBRARY_PATH` to make sure the `.so` libraries of that local install are used (and not the libraries from the local distribution install, e.g., `/usr/lib/swi-prolog`).

To use the SWIPL in the Linux distribution, located in `/usr/lib/swi-prolog`:

```shell script
[ssardina@Thinkpad-X1 system]$ SWI_HOME_DIR=/usr/lib/swi-prolog \
  SWI_EXEC_FILE=/usr/lib/swi-prolog/bin/x86_64-linux/swipl \
  SWIPL_BOOT_FILE=/usr/lib/swi-prolog/boot.prc \
  java -cp ../:/usr/lib/swi-prolog/lib/jpl.jar system.Init
```



----

## Contributions

* Paul Singleton (paul.singleton@bcs.org.uk)
    * July 2003
    * January 2004 (revised)
    * December 2004 (not revised much apart from this line)

* Sebastian Sardina (ssardina@gmail.com)
    * June 2020 (refactored, updated, extended)

