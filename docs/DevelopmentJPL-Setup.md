# Setting up SWIPL & JPL for development

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


