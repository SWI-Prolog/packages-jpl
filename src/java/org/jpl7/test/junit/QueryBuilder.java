package org.jpl7.test.junit;

import org.jpl7.*;
import org.jpl7.Float;
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

public class QueryBuilder {
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
    public void testTerm1() {
        Term args = Util.textToTerm("[1,2,3,4,5]");
        Term t = new Compound("member", new Term[] { new Integer(1), args } );
        Query q = new Query(t);
        assertTrue("Query should have succeded, but it did not!", q.hasSolution());
    }

    // Query term is not an Atom or Compound, but an Float!
    @Test
    public void testTermErr1() {
        Term t = new Float(1.23);
        try {
            Query q = new Query(t);
            fail("Query should have given JPLException");
        } catch (JPLException e) {
            // all good!
            System.out.println("\t" + e.getMessage());
        } catch (Exception e) {
            fail("Query should have given JPLException");
        }
    }










    @Test
    public void testString1() {
        Query q = new Query("member(1, [1,2,3])");
        assertTrue("Query should have succeded, but it did not!", q.hasSolution());
    }

    @Test
    public void testString2() {
        Query q = new Query("atom(?)", new Atom("hello"));
        assertTrue("Query should have succeded, but it did not!", q.hasSolution());
    }

    @Test
    public void testString3() {
        Term[] args = new Term[] { new Integer(1), Util.textToTerm("[1,2,3,4,5]") };

        Query q = new Query("member(?, ?)", args);
        assertTrue("Query should have succeded, but it did not!", q.hasSolution());
    }

    @Test
    public void testString4() {
        Term[] args = new Term[] { new Integer(1), Util.textToTerm("[1,2,3,4,5]") };

        Query q = new Query("member", args);
        assertTrue("Query should have succeded, but it did not!", q.hasSolution());
    }

    @Test
    public void testString5() {
        Query q = new Query("atom", new Atom("hello"));
        assertTrue("Query should have succeded, but it did not!", q.hasSolution());
    }



    // Query term is not an Atom or Compound, but an Integer!
    @Test
    public void testStringErr1() {
        try {
            Query q = new Query("112");
            fail("Query should have given JPLException");
        } catch (JPLException e) {
            // all good!
            System.out.println("\t" + e.getMessage());
        } catch (Exception e) {
            fail("Query should have given JPLException");
        }
    }

    // Query text is not a proper term
    @Test
    public void testStringErr2() {
        try {
            Query q = new Query("112(sas,23");
            fail("Query should have given PrologException: malformed query");
        } catch (PrologException e) {
            // all good!
            System.out.println("\t" + e.getMessage());
        } catch (JPLException e) {
            fail("Should have been caught before because JPLException is a subclass of PrologException");
        } catch (Exception e) {
            fail("Query should have given JPLException");
        }
    }

    // Error in number of placeholder matching arguments (too many terms)
    @Test
    public void testStringErr3() {
        Term[] args = new Term[] { new Integer(1), Util.textToTerm("[1,2,3,4,5]") };

        try {
            Query q = new Query("member(?, ?, ?)", args);
            fail("Query should have given JPLException");
        } catch (JPLException e) {
            // all good!
            System.out.println("\t" + e.getMessage());
        } catch (Exception e) {
            fail("Query should have given JPLException");
        }
    }

    // Error in number of placeholder matching arguments (too many terms)
    @Test
    public void testStringErr4() {
        Term[] args = new Term[] { new Integer(1), Util.textToTerm("[1,2,3,4,5]") };

        try {
            Query q = new Query("member(?)", args);
            fail("Query should have given JPLException");
        } catch (JPLException e) {
            // all good!
            System.out.println("\t" + e.getMessage());
        } catch (Exception e) {
            fail("Query should have given JPLException");
        }
    }

}
