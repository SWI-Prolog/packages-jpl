# Developing JPL

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



## Developing from an IDE

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
