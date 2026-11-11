package org.jpl7.junit;

import java.math.BigInteger;
import org.jpl7.*;
import org.jpl7.Integer; // prefer to java.lang.Integer
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

public class Test_Integer extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.jpl7.JPLTestSuiteRunner.run(Test_Integer.class); // full name with package
    }

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
        assertTrue(x.isInteger(), "X is an org.jpl7.Integer");
        assertTrue(x.isBigInteger(), "X is a big integer");
        assertEquals(x.bigValue(), b, "X's big value is 51**51");
    }

    @Test
    public void testBigInteger2() {
        BigInteger b = new BigInteger("12345678901234567890123456789");
        Term i = new Integer(b); // too big for a long
        Term g = new Compound("is", new Term[]{new Variable("X"), i});
        Term x = Query.oneSolution(g).get("X");
        assertTrue(x.isInteger(), "X is an org.jpl7.Integer");
        assertTrue(x.isBigInteger(), "X is a big org.jpl7.Integer");
        assertEquals(x.bigValue(), b, "X's value is as expected");
    }
}
