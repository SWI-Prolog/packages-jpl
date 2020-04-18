package org.jpl7.test.junit;

import org.jpl7.Atom;
import org.jpl7.Integer;
import org.jpl7.Query;
import org.jpl7.Term;
import org.junit.*;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runners.MethodSorters;

import static org.junit.Assert.*;


@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class MutualRecursion extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.test.junit.MutualRecursion");

        // should work from static class but gives error
//        org.junit.runner.JUnitCore.main( GetSolution.class.getName()); // full name with package
    }


    /**
     * This is done at the class loading, before any test is run
     */
    @BeforeClass
    public static void setUp() {
        setUpClass();

        consultTestFile();
//        useJPLmodule();     // consult the jpl.pl module
    }


    @Rule
    public TestRule watcher = new TestWatcher() {
        protected void starting(Description description) {
            reportTest(description);
        }
    };





    ///////////////////////////////////////////////////////////////////////////////
    // SUPPORTING CODE
    ///////////////////////////////////////////////////////////////////////////////

    public static long fac(long n) { // complements
        // jpl:jpl_test_fac(+integer,-integer);
        // indirectly supports
        // testMutualRecursion
        consultTestFile();

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

    private void testMutualRecursion(int n, long f) {
        // f is the expected result for call fac(n) which uses Prolog
        try {
            assertEquals(String.format("Mutual recursive Java<->Prolog factorial: fac(%d) = %d", n, f), f, fac(n));
        } catch (Exception e) {
            fail(String.format("fac(%d) threw %s", n, e));
        }
    }

    @Test
    public void testMutualRecursion01() {
        testMutualRecursion(1, 1);
    }

    @Test
    public void testMutualRecursion02() {
        testMutualRecursion(2, 2);
    }

    @Test
    public void testMutualRecursion03() {
        testMutualRecursion(3, 6);
    }

    @Test
    public void testMutualRecursion10() {
        testMutualRecursion(10, 3628800);
    }


}
