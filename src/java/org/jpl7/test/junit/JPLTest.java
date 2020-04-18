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

abstract class JPLTest {
    public static final String home =
            (System.getenv("SWI_HOME_DIR") == null ? "../../build/home/"
                    : System.getenv("SWI_HOME_DIR"));
    public static final String startup  =
            (System.getenv("SWIPL_BOOT_FILE") == null ? String.format("%s/boot.prc", home)
                    : System.getenv("SWIPL_BOOT_FILE"));
    public static final String syntax =
            (System.getenv("SWIPL_SYNTAX") == null ? "modern"
                    : System.getenv("SWIPL_SYNTAX"));
    public static final String test_dir =
            (System.getenv("TEST_DIR") == null ? "src/java/org/jpl7/test"
                    : System.getenv("TEST_DIR"));
    public static final String test_jpl = String.format("%s/test_jpl.pl", test_dir);
    public static final boolean report =
            (System.getenv("REPORT") == null ? true
                    : System.getenv("REPORT") == "true");




    protected static void setUpClass() {
        if (syntax.equals("traditional")) {
            JPL.setTraditional();
            Prolog.set_default_init_args(new String[] {
//					"libswipl.dll", "-x", startup, "-f", "none",
                    "libswipl.dll", "-f", "none",
                    "-g", "true", "--traditional", "-q",
                    "--home="+home, "--no-signals", "--no-packs" });
        } else {
            Prolog.set_default_init_args(new String[] {
//					"libswipl.dll", "-x", startup, "-f", "none",
                    "libswipl.dll", "-f", "none",
                    "-g", "true", "-q",
                    "--home="+home, "--no-signals", "--no-packs" });
        }
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
