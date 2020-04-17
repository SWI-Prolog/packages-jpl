package org.jpl7.test.junit;

import org.jpl7.*;
import org.jpl7.Integer;
import org.jpl7.fli.Prolog;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.math.BigInteger;

import static org.junit.Assert.*;


public class IntegerTest {
    public static final String startup = (System.getenv("SWIPL_BOOT_FILE") == null ? "../../src/swipl.prc"
            : System.getenv("SWIPL_BOOT_FILE"));
    public static final String test_jpl = (System.getenv("TEST_JPL") == null ? "test_jpl.pl"
            : System.getenv("TEST_JPL"));
    public static final String syntax = (System.getenv("SWIPL_SYNTAX") == null ? "modern"
            : System.getenv("SWIPL_SYNTAX"));
    public static final String home = (System.getenv("SWI_HOME_DIR") == null ? "../.."
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
    public void testIntegerFromByte1() {
        byte b = (byte) 127; // -128..127
        Integer i = new Integer(b);
        assertTrue(i.intValue() == b);
    }

    @Test
    public void testIntegerFromChar1() {
        char c = (char) 64; // 0..65535
        // System.out.println("c = " + c);
        Integer i = new Integer(c);
        assertTrue(i.intValue() == c);
    }

    @Test
    public void testInteger1() {
        try {
            Term i = Query.oneSolution("I is 2**40").get("I"); // long but not
            // int
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
        // System.out.println("X.bigValue() = " + x.bigValue().toString());
        // System.out.println("b.bigValue() = " + b.toString());
        assertTrue("X is a big integer", x.isBigInteger());
        assertTrue("X's big value is 51**51", x.bigValue().equals(b));
    }

    @Test
    public void testBigInteger2() {
        BigInteger b = new BigInteger("12345678901234567890123456789");
        Term i = new Integer(b); // too big for a long
        Term g = new Compound("is", new Term[]{new Variable("X"), i});
        Term x = Query.oneSolution(g).get("X");
        assertTrue("X is an org.jpl7.Integer", x.isInteger());
        assertTrue("X is a big org.jpl7.Integer", x.isBigInteger());
        assertTrue("X's value is as expected", x.bigValue().equals(b));
    }


}
