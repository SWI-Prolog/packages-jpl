package org.jpl7.test.junit;


import org.jpl7.Atom;
import org.jpl7.JPL;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.fli.Prolog;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.util.Arrays;

abstract class JPLTest {
    public static final String home =
            (System.getenv("SWI_HOME_DIR") == null ? "../../build/home/"
                    : System.getenv("SWI_HOME_DIR"));
    public static final String startup  =
            (System.getenv("SWIPL_BOOT_FILE") == null ? String.format("%s/boot.prc", home)
                    : System.getenv("SWIPL_BOOT_FILE"));

    //    public static final String swi_exec = String.format("%s/../src/swipl", home);
    // Somehow Windows requires libswipl.dll (https://github.com/SWI-Prolog/packages-jpl/issues/32)
    //  but if we use that in Linux, then swipl.rc does not load the search paths correctly
    public static final String swi_exec = String.format("%s/../src/swipl", home);

    public static final String syntax =
            (System.getenv("SWIPL_SYNTAX") == null ? "modern"
                    : System.getenv("SWIPL_SYNTAX"));
    public static final String source_dir =
            System.getenv("SOURCE_DIR") == null ? "." : System.getenv("SOURCE_DIR");
    public static final String test_dir = String.format("%s/src/java/org/jpl7/test", source_dir);

    // test_jpl is the main testing file for jpl, used also for java_in_prolog
    //  it is in the original source tree (not in the build), but ../../packages/jpl = current dir so it works
    //      both for CMAKE and for development in packags/jpl (e.g., IntelliJ)
    public static final String test_jpl = String.format("%s/test_jpl.pl", source_dir);
    public static final boolean report =
            (System.getenv("REPORT") == null ? true
                    : System.getenv("REPORT") == "true");


    protected static void setUpClass() {
        if (syntax.equals("traditional")) {
            JPL.setTraditional();
        }

        // CLI options for SWI: https://www.swi-prolog.org/pldoc/man?section=cmdline
        // This is how the SWI engine will be configured/set-up
        //  See this issue on how this line came up: https://github.com/SWI-Prolog/packages-jpl/issues/32
        //  swi_exec is not anymore "libswipl.dll" which was a "flag" to avoid setting up the search paths for
        //      the .so libraries in swipl.rc: now it sets up those paths always
        String init_swi_config =
                String.format("%s -x %s -F swipl --home=%s -f none -g true -q --no-signals --no-packs",
                        swi_exec, startup, home);

        // Configure the SWI engine before starting - both statements are equivalent
        JPL.setDefaultInitArgs(init_swi_config.split("\\s+"));
//        Prolog.set_default_init_args(init_swi_config.split("\\s+"));
    }


    // This is how the test is reported
    protected void reportTest(Description description, String msg) {
            if (report) {
                System.out.println(String.format("Starting test: %s (%s)",
                        description.getMethodName(),
                        description.getTestClass().getSimpleName()));
            }
    };

    // This is how the test is reported
    protected void reportTest(Description description) {
        reportTest(description, "");
    };

    protected static void consultTestFile() {
//        Query q_load_test_jplnew Query("consult", new Term[] { new Atom(test_jpl) })
        Query q_load_test_jpl = new Query(String.format("consult('%s')", test_jpl));
        q_load_test_jpl.hasSolution();
    }

    protected static void useJPLmodule() {
        Query.hasSolution("use_module(library(jpl))"); // only because we call e.g. jpl_pl_syntax/1 below

//        Query q_load_jpl = new Query("use_module(library(jpl))");
//        q_load_jpl.hasSolution();
    }


    ///////////////////////////////////////////////////////////////////////////////
    // SUPPORTING CODE
    ///////////////////////////////////////////////////////////////////////////////




}