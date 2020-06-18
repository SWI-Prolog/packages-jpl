# Release Notes - 7.6.0

Uses Java source 1.7.

- New `org.jpl7.Rational` type to handle [SWI rationals](https://www.swi-prolog.org/pldoc/man?section=rational).
- Refactored several methods dealing with JPL terms, from `org.jpl7.Util` to `org.jpl7.Term`:
    - `Term textToTerm(String text)`
    - `String[] atomListToStringArray(Term t)` 
    - `static Term intArrayArrayToList(int[][] a)`
    - `Term intArrayToList(int[] a)`
    - `boolean isList(Term term)`
    - `int listToLength(Term term)`
    - `Term[] listToTermArray(Term t)`
    - `Term stringArrayToList(String[] a)`
    - `Term termArrayToList(Term[] terms)`
- Added a textual mode for `Term.toString()` to convert non-empty lists in Prolog textual style `[e2, e2, ..., en]` instead of the pre-fix functor-based style `'[|]'(e1, '[|]'(e2, '[|]'(...,'[|]'(en,'[]')..)`.
    - This textual mode is used when `JPL.LIST_TOSTRING_TEXTUAL` is True (default is True); otherwise default pre-fix style is used.
- Added specific section for lists in documentation.
        
## Internal

- More direct and simpler `Term.textToTerm(String text)` without using 
`getSolutionWithVarNames` and by renaming anonymous Variable terms to give them the textual name.
- Migrated unit testing from JUnit3 to JUnit4. 
    - Refactored the unit testing test suite; all test files subclass of `org.jpl7.java.test.junit.JPLTest`.
- Modified init arguments and CMAKE configuration for SWI embeded engine unit testing to fix issue with engine not loading libraries. No more use of `libswipl.dll` as first argument; all packages available in unit tetsing now.
- Added some static versions of instance methods in class `Term`.