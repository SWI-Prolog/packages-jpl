package org.jpl7.test.junit;

import org.jpl7.Atom;
import org.jpl7.Integer;
import org.jpl7.Query;
import org.jpl7.Term;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import static org.junit.Assert.*;


public class MutualRecursion {
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

    @BeforeClass
    public static void setUp() {
        // JPL.setTraditional();

        // Consult this file to have access to jpl_test_fac/2
        assertTrue((new Query("consult", new Term[] { new Atom(test_jpl) })).hasSolution());
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

    public static long fac(long n) { // complements
        // jpl:jpl_test_fac(+integer,-integer);
        // indirectly supports
        // testMutualRecursion
        if (n == 1) {
            return 1;
        } else if (n > 1) {
            return n * ((Integer) Query
                    .oneSolution("jpl_test_fac(?,F)", new Term[]{new Integer(n - 1)}).get("F")).longValue();
        } else {
            return 0;
        }
    }



    ///////////////////////////////////////////////////////////////////////////////
    // TESTS
    ///////////////////////////////////////////////////////////////////////////////




    private void testMutualRecursion(int n, long f) { // f is the expected
        // result for fac(n)
        try {
            assertEquals("mutual recursive Java<->Prolog factorial: fac(" + n + ") = " + f, fac(n), f);
        } catch (Exception e) {
            fail("fac(" + n + ") threw " + e);
        }
    }

    @Test
    public void testMutualRecursion1() {
        testMutualRecursion(1, 1);
    }

    @Test
    public void testMutualRecursion2() {
        testMutualRecursion(2, 2);
    }

    @Test
    public void testMutualRecursion3() {
        testMutualRecursion(3, 6);
    }

    @Test
    public void testMutualRecursion10() {
        testMutualRecursion(10, 3628800);
    }


}
