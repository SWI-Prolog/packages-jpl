package org.jpl7.test.junit;

import org.jpl7.Atom;
import org.jpl7.JPL;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.fli.Prolog;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;


public class StringTest {
    public static final String startup =
            (System.getenv("SWIPL_BOOT_FILE") == null ? "../../src/swipl.prc"
                    : System.getenv("SWIPL_BOOT_FILE"));
    public static final String test_jpl =
            (System.getenv("TEST_JPL") == null ? "test_jpl.pl"
                    : System.getenv("TEST_JPL"));
    public static final String syntax =
            (System.getenv("SWIPL_SYNTAX") == null ? "modern"
                    : System.getenv("SWIPL_SYNTAX"));
    public static final String home =
            (System.getenv("SWI_HOME_DIR") == null ? "../.."
                    : System.getenv("SWI_HOME_DIR"));

    public static void main(String argv[]) {
    }



    ///////////////////////////////////////////////////////////////////////////////
    // TESTING CONFIGURATION
    ///////////////////////////////////////////////////////////////////////////////

    /**
     * This is done at the class loading, before any test is run
     */
    @BeforeClass
    public static void setUp() {
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

    @Rule
    public TestRule watcher = new TestWatcher() {
        protected void starting(Description description) {
//            logger.info("{} being run...", description.getMethodName());

            System.out.println("Starting test: " + description.getMethodName());
        }
    };


    ///////////////////////////////////////////////////////////////////////////////
    // SUPPORTING CODE
    ///////////////////////////////////////////////////////////////////////////////



    ///////////////////////////////////////////////////////////////////////////////
    // TESTS
    ///////////////////////////////////////////////////////////////////////////////



    @Test
    public void testStringXput1() {
        Term a = Query.oneSolution("string_concat(foo,bar,S)").get("S");
        assertEquals("foobar", a.name());
        assertEquals("string", a.atomType());
    }

    @Test
    public void testStringXput2() {
        String s1 = "\u0000\u007F\u0080\u00FF";
        String s2 = "\u0100\u7FFF\u8000\uFFFF";
        String s = s1 + s2; // concatenate in Java
        Term a = Query.oneSolution("string_concat(?,?,S)", new Term[]{new Atom(s1), new Atom(s2)}).get("S"); // concatenate
        // in
        // Prolog
        assertEquals(s, a.name());
        assertEquals("string", a.atomType());
    }



}
