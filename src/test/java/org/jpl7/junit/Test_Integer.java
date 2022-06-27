package org.jpl7.junit;

import org.jpl7.*;
import org.jpl7.Integer;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.math.BigInteger;

import static org.junit.Assert.*;


public class Test_Integer extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.junit.Test_Integer");

        // should work from static class but gives error
//        org.junit.runner.JUnitCore.main( GetSolution.class.getName()); // full name with package
    }

    /**
     * This is done at the class loading, before any test is run
     */
    @BeforeClass
    public static void setUp() {
        setUpClass();
    }

    @Rule
    public TestRule watcher = new TestWatcher() {
        protected void starting(Description description) {
            reportTest(description);
        }
    };

    @Test
    public void testIntegerFromByte1() {
        byte b = (byte) 127; // -128..127
        Integer i = new Integer(b);
        assertEquals(i.intValue(), b);
    }

    @Test
    public void testIntegerFromChar1() {
        char c = (char) 64; // 0..65535
        Integer i = new Integer(c);
        assertEquals(i.intValue(), c);
    }

    @Test
    public void testInteger1() {
        try {
            Term i = Query.oneSolution("I is 2**40").get("I"); // long but not int
            i.intValue();
            fail("intValue() of bigger-than-int value failed to throw an exception");
        } catch (JPLException e) {
            if (e.getMessage().endsWith("cannot represent value as an int")) {
                // OK: an appropriate exception was thrown
            } else {
                fail("intValue() of bigger-than-int value threw incorrect JPLException: " + e);
            }
        } catch (Exception e) {
            fail("intValue() of bigger-than-int value threw unexpected class of exception: " + e);
        }
    }

    @Test
    public void testBigInteger1() {
        BigInteger a = new BigInteger(Long.toString(51L));
        BigInteger b = a.pow(51); // 51**51, too big for a long
        Term x = Query.oneSolution("X is 51**51").get("X");
        assertTrue("X is an org.jpl7.Integer", x.isInteger());
        assertTrue("X is a big integer", x.isBigInteger());
        assertEquals("X's big value is 51**51", x.bigValue(), b);
    }

    @Test
    public void testBigInteger2() {
        BigInteger b = new BigInteger("12345678901234567890123456789");
        Term i = new Integer(b); // too big for a long
        Term g = new Compound("is", new Term[]{new Variable("X"), i});
        Term x = Query.oneSolution(g).get("X");
        assertTrue("X is an org.jpl7.Integer", x.isInteger());
        assertTrue("X is a big org.jpl7.Integer", x.isBigInteger());
        assertEquals("X's value is as expected", x.bigValue(), b);
    }
}
