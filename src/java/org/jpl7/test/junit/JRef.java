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

import static org.junit.Assert.*;


public class JRef {
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

        assertTrue((new Query("consult", new Term[] { new Atom(test_jpl) })).hasSolution());
        assertTrue((new Query("ensure_loaded(library(jpl))")).hasSolution());

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
    public void testJRef1() {
        int i = 76543;
        Integer I = new Integer(i);
        Query q = new Query("jpl_call(?,intValue,[],I2)", new Term[] { JPL.newJRef(I) });
        Term I2 = q.oneSolution().get("I2");
        assertTrue(I2.isInteger() && I2.intValue() == i);
    }

    @Test
    public void testJRef2() {
        int i = 76543;
        Integer I = new Integer(i);
        Query q = new Query("jpl_call(?,intValue,[],I2)", JPL.newJRef(I));
        Term I2 = q.oneSolution().get("I2");
        assertTrue(I2.isInteger() && I2.intValue() == i);
    }

    @Test
    public void testJRef3() {
        StringBuffer sb = new StringBuffer();
        Query.oneSolution("jpl_call(?,append,['xyz'],_)", new Term[] { JPL.newJRef(sb) });
        assertTrue(sb.toString().equals("xyz"));
    }

    @Test
    public void testJRef4() {
        Term jrefSB = Query.oneSolution("jpl_new('java.lang.StringBuffer',['abc'],SB)").get("SB");
        assertTrue(jrefSB.isJRef() && ((StringBuffer) jrefSB.object()).toString().equals("abc"));
    }

    @Test
    public void testJRef5() {
        String token = "foobar345";
        Term a = Query.oneSolution("jpl_new('java.lang.StringBuffer',[?],A)", new Term[] { new Atom(token) }).get("A");
        assertTrue(((java.lang.StringBuffer) (a.object())).toString().equals(token));
    }



    @Test
    public void testRef6() {
        Term nullJRef = JPL.newJRef(null);
        Object nullObject = nullJRef.object();
        assertNull("JPL null Term yields a null object", nullObject);
    }


    @Test
    public void testRef7() {
        Term badJRef = new Compound("hello", new Term[] { new Atom("foobar") }); // term hello(foobar)
        try {
            badJRef.object(); // should throw exception
            fail("@(foobar).object() should thrown JPLException"); // shouldn't get to here
        } catch (JPLException e) { // expected exception class
            if (e.getMessage().endsWith("term is not a JRef")) {
                // OK: an appropriate exception was thrown
            } else {
                fail("hello(foobar).object() threw wrong JPLException: " + e);
            }
        } catch (Exception e) {
            fail("hello(foobar).object() threw wrong exception class: " + e);
        }
    }


}
