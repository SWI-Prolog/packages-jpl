# Examples for JPL 7.6.0

This directory contains various example tests for JPL's Java-calls-Prolog interface.

All examples are within package `org.jpl7.examples`.

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

Tu run any example, we need to provide the location of `jpl.jar`.

For example, to run a particular example `X` that does not require any resources (e.g., consulting a Prolog file) we can simply do:

```bash
java -cp .:/usr/lib/swi-prolog/lib/jpl.jar  org.jpl7.examples.X
```

If the example requires some resource, such as loading a Prolog `.pl` file, we need to cd into the location of the example and run it from there so that the resource is in the current user dir. For example:

```bash
cd org/jpl7/examples/family/
java -cp ../../../../:/usr/lib/swi-prolog/lib/jpl.jar org.jpl7.examples.family.Family
```


## Examples

Exceptions
	demonstrates how an uncaught Prolog exception is turned
	into a Java exception (also uncaught, in this example)

Exceptions2
	demonstrates how an uncaught Prolog exception is turned
	into a Java exception, which in this example is caught,
	converted to a String and printed on System.out (and
	also shown in a Swing treeview gadget)

### Family

There are two variants in package `org.jpl7.examples.family`

The class `Family` consults some parent-child relationship clauses (from `family.pl`) and runs some simple queries.

```
# cd into org/jpl7/examples/family
java -cp ../../../../:/usr/lib/swi-prolog/lib/jpl.jar org.jpl7.examples.family.Family
```


In turn, class `FamilyMT` consults the same parent-child relationship clauses (again, from `family.pl`) and then sets off twenty threads, each doing the
same queries of `Family`.  As there are only 10 available Prolog engines by default, the threads sometimes get to wait.

```
# cd into org/jpl7/examples/family
java -cp ../../../../:/usr/lib/swi-prolog/lib/jpl.jar org.jpl7.examples.family.FamilyMT
```
### Test
	
Runs a simple set of tests of the High-Level Interface. You should run this test once you have gotten JPL built, or if you are experiencing any problems with the package.

The test loads a `test.pl` Prolog file.

```
# cd into org/jpl7/examples/test
java -cp ../../../../:/usr/lib/swi-prolog/lib/jpl.jar org.jpl7.examples.test.Test
```

### Factorial

This example is in `org.jpl7.examples.factorial` package and consults file `fact.pl`.

The example recursively computes `factorial(10)` by alternately calling Prolog from Java and Java from Prolog.

```bash
# First, cd into org/jpl7/examples/factorial

[ssardina@Thinkpad-X1 factorial]$ java -cp ../../../../:/usr/lib/swi-prolog/lib/jpl.jar org.jpl7.examples.factorial.Factorial
calling Prolog to call Java to call Prolog...
factorial(10) = 3628800
```


Time
	runs a set of timing experiments, designed to gauge the speed
	with which terms are exchanged between Java and Prolog

### Versions

The `org.jpl7.Versions` class example tries to discover which versions (e.g. `7.6.0-alpha`) of each JPL library (i.e. Prolog, C and Java) are installed, and congratulates you if they are the same. It also reports the home and version of the SWIPL engine being used.

Zahed
	creates a trivial-in-Prolog-but-horrible-in-JPL1.x query goal
	and calls it against a trivial Prolog database; this would be much
	nicer if it used recent facilities for creating queries from
	Prolog source text fragments

NB each folder contains a Java application which should be run within
its folder so it can find any corresponding .pl (Prolog source) files:
Windows users can use 'compile.bat' to (re)compile each demo, and
'run.bat' to run it.

----

Paul Singleton (paul.singleton@bcs.org.uk)
July 2003
January 2004 (revised)
December 2004 (not revised much apart from this line)