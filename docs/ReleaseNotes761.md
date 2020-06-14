# Release Notes - 7.6.1

- New `org.jpl7.Dict` type to handle [SWI dictionaries](https://www.swi-prolog.org/pldoc/man?section=bidicts).
- Refactored into [Maven-compatible standard directory layout](https://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html).
    - This included application sources in `src/main/` and tests in `src/test/`
- Make repo Maven based.
- Clean-up, fixed, and updated all examples:
    - Refactored all examples into `src/examples`.
    - New README file under `src/examples/java` with explanations for each Java-calls-Prolog example.
    - Added some more useful examples and repaired non-working ones.

        