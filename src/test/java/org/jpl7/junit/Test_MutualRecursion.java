package org.jpl7.junit;

import org.jpl7.*;
import org.jpl7.Integer;
import static org.junit.Assert.*;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class Test_MutualRecursion extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main(Test_MutualRecursion.class.getName()); // full name with package
    }

    /**
     * Complements jpl:jpl_test_fac(+integer,-integer) to compute n! by mutual recursion between Java and Prolog
     * @param n whose factorial n! is to be computed
     * @return n! 
     */
    public static long fac(long n) {
        if (n == 1) {
            return 1;
        } else if (n > 1) {
            return n * ((Integer) Query
                    .oneSolution("jpl_test_fac(?,F)", new Term[]{new Integer(n - 1)}).get("F")).longValue();
        } else {
            return 0;
        }
    }

    /**
     * @param n whose factorial n! is to be computed by mutual recursion between Java and Prolog
     * @param f expected result of fac(n)
     */
    private void testMutualRecursion(int n, long f) {
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
