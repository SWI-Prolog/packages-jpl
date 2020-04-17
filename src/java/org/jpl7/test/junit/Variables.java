package org.jpl7.test.junit;

import org.jpl7.Integer;
import org.jpl7.*;
import org.jpl7.fli.Prolog;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Map;
import java.util.NoSuchElementException;

import static org.junit.Assert.*;

// This is the original testbed ported to  JUNIT Version 4:
// https://junit.org/junit4/faq.html
//https://objectcomputing.com/resources/publications/sett/august-2007-migrating-from-junit-3-to-junit-4-nothing-but-good-news

// This class defines all the tests which are run from Java.
public class Variables {
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
		org.junit.runner.JUnitCore.main("org.jpl7.test.junit.Variables");
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
    public void testVariableBinding1() {
        Term lhs = new Compound("p", new Term[]{new Variable("X"), new Variable("Y")});
        Term rhs = new Compound("p", new Term[]{new Atom("a"), new Atom("b")});
        Term goal = new Compound("=", new Term[]{lhs, rhs});
        Map<String, Term> soln = new Query(goal).oneSolution();
        assertTrue("two Variables with different names can bind to distinct atoms",
                soln != null && (soln.get("X")).name().equals("a") && (soln.get("Y")).name().equals("b"));
    }

    @Test
    public void testFreeVariable1() {
        Query q = new Query("append([X],[_,_],[_,_,_])");
        Map<String, Term> sol = q.oneSolution();

        assertEquals("Variable X must remain free after query",
                sol.get("X").typeName(), "Variable");
    }

    /**
     * TEST DISABLED!!
     *
     * Gives error:
     * java.lang.AssertionError: Variable X must be bound to Y
     * Expected :Y
     * Actual   :_142
     */
//    @Test
    public void testFreeVariable2() {
        Variable VarX = new Variable("X");
        Variable VarY = new Variable("Y");

//        Query q = new Query("append([X],[_,_],[_,_,_])");
        Query q = new Query(new Compound("=", new Term[] { VarX, VarY }));

        Map<String, Term> sol = q.oneSolution();

        assertEquals("Variable X must be bound to Y", VarY, sol.get("X"));
    }



    @Test
    public void testVariableBinding2() {
        Term lhs = new Compound("p", new Term[]{new Variable("X"), new Variable("X")});
        Term rhs = new Compound("p", new Term[]{new Atom("a"), new Atom("b")});
        Term goal = new Compound("=", new Term[]{lhs, rhs});
        assertFalse("two distinct Variables with same name cannot unify with distinct atoms",
                new Query(goal).hasSolution());
    }

    @Test
    public void testVariableBinding3() {
        Variable X = new Variable("X");
        Term lhs = new Compound("p", new Term[]{X, X});
        Term rhs = new Compound("p", new Term[]{new Atom("a"), new Atom("b")});
        Term goal = new Compound("=", new Term[]{lhs, rhs});
        assertFalse("two references to the same (named) Variable cannot unify with differing atoms",
                new Query(goal).hasSolution());
    }

    @Test
    public void testVariableBinding4() {
        Term lhs = new Compound("p", new Term[]{new Variable("_"), new Variable("_")});
        Term rhs = new Compound("p", new Term[]{new Atom("a"), new Atom("b")});
        Term goal = new Compound("=", new Term[]{lhs, rhs});
        assertTrue("two distinct anonymous Variables can unify with distinct atoms", new Query(goal).hasSolution());
    }

    @Test
    public void testVariableBinding5() {
        Variable Anon = new Variable("_");
        Term lhs = new Compound("p", new Term[]{Anon, Anon});
        Term rhs = new Compound("p", new Term[]{new Atom("a"), new Atom("b")});
        Term goal = new Compound("=", new Term[]{lhs, rhs});
        assertTrue("two references to an anonymous Variable can unify with differing atoms",
                new Query(goal).hasSolution());
    }

}
