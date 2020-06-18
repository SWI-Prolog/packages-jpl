# Developing JPL

If you want to develop JPL further you will need to:

1. Understand the different components of JPL. There is Java, C and Prolog code involved in JPL.
2. Set-up the source tree of JPL package within the main [SWIPL source tree](https://github.com/SWI-Prolog/swipl-devel).
    * The whole main SWIPL source trees is required to compile the whole framework using CMAKE and thus generate the C libraries.
3. Compile and unit tests the compiled version using CMAKE.
4. Using SWIPL and JPL from the compiled, but uninstalled, system.
5. Contribute changes to main [packges-jpl](https://github.com/SWI-Prolog/packages-jpl) repository.

