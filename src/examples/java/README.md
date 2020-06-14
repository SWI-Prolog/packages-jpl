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

Tu **run the examples** we need to:
 
 * provide the location of `jpl.jar`, always. 
 * run it within its folder so it can find any corresponding resource `.pl` (Prolog source) files, if any.

For example, to run a particular example `X` that does _not_ require any resources (e.g., consulting a Prolog file) we can simply do:

```bash
java -cp .:/usr/lib/swi-prolog/lib/jpl.jar  org.jpl7.examples.X
```

If the example requires some resource, such as loading a Prolog `.pl` file, we need to cd into the location of the example and run it from there so that the resource is in the current user dir. For example:

```bash
cd org/jpl7/examples/family/
java -cp ../../../../:/usr/lib/swi-prolog/lib/jpl.jar org.jpl7.examples.family.Family
```


## Examples

## Exceptions

Demonstrates how an uncaught Prolog exception is turned into a Java exception, which in this example is caught in Java.

```
[ssardina@Thinkpad-X1 java]$ java -cp .:/usr/lib/swi-prolog/lib/jpl.jar  org.jpl7.examples.Exceptions
calling

?- X is Y.

in Prolog to force a Prolog 'instantiation_error' exception,
which should be returned via Java as an uncaught org.jpl7.PrologException in thread "main".

BINGO! The following exception was thrown: 
	PrologException: error(instantiation_error, context(':'(system, '/'(is, 2)), _2))
```



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


### Time
	
Runs a set of timing experiments, designed to gauge the speed with which terms are exchanged between Java and Prolog.

```bash
[ssardina@Thinkpad-X1 time]$ java -cp ../../../../:/usr/lib/swi-prolog/lib/jpl.jar org.jpl7.examples.time.Time
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

### Versions

The `org.jpl7.Versions` class example tries to discover which versions (e.g. `7.6.0-alpha`) of each JPL library (i.e. Prolog, C and Java) are installed, and congratulates you if they are the same. It also reports the home and version of the SWIPL engine being used.

### Zahed
	
Creates a trivial-in-Prolog-but-horrible-in-JPL query goal and calls it against a trivial Prolog database (file `zahed.pl`).

The query term is built "from first principles", by composing the various data types one by one. Of course, in the new JPL, one uses recent facilities for creating query terms from Prolog source text fragments (namely, `Term.textToTerm()`).

```
[ssardina@Thinkpad-X1 zahed]$ java -cp ../../../../:/usr/lib/swi-prolog/lib/jpl.jar org.jpl7.examples.zahed.Zahed
starting...
The query constructed is: gen([t(c, q, []), t(v, [], a)], A)
ok
finished
```

----

# Contributions

Paul Singleton (paul.singleton@bcs.org.uk)
July 2003
January 2004 (revised)
December 2004 (not revised much apart from this line)

Sebastian Sardina (ssardina@gmail.com)
June 2020