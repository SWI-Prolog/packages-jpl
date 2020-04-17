package org.jpl7.test.junit;

import org.jpl7.Integer;
import org.jpl7.*;
import org.jpl7.fli.Prolog;

// This is the original testbed ported to  JUNIT Version 4:
// https://junit.org/junit4/faq.html
//https://objectcomputing.com/resources/publications/sett/august-2007-migrating-from-junit-3-to-junit-4-nothing-but-good-news
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.lang.reflect.Array;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Map;
import java.util.NoSuchElementException;

import static org.junit.Assert.*;

// This class defines all the tests which are run from Java.
public class Tests {
    public static final String syntax = (
            System.getenv("SWIPL_SYNTAX") == null ? "modern"
            : System.getenv("SWIPL_SYNTAX"));
    public static final String startup =
            (System.getenv("SWIPL_BOOT_FILE") == null ? "../../src/swipl.prc"
            : System.getenv("SWIPL_BOOT_FILE"));
    public static final String test_jpl =
            (System.getenv("TEST_JPL") == null ? "test_jpl.pl"
            : System.getenv("TEST_JPL"));
    public static final String home =
            (System.getenv("SWI_HOME_DIR") == null ? "../.."
            : System.getenv("SWI_HOME_DIR"));



    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.test.junit.Tests");
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

		reportConfiguration();

		// These two lines are from old testing set the second one does not work and makes it all fail
        // Leave it here as example on how to use those native functions
//        Prolog.set_default_init_args(
//                new String[] { "libpl.dll", "-f", "none", "-g", "set_prolog_flag(debug_on_error,false)", "-q" });
//        System.out.println("tag = " + Prolog.object_to_tag(new Query("hello")));

    }

    private static void reportConfiguration() {
        System.out.println("JPL testing under the following configuration:");

        Query.hasSolution("use_module(library(jpl))"); // only because we call e.g. jpl_pl_syntax/1 below
        System.out.println("Module library(jpl) loaded");

        Term swi = Query.oneSolution("current_prolog_flag(version_data,Swi)").get("Swi");
        System.out.println("\t swipl.version = " + swi.arg(1) + "." + swi.arg(2) + "." + swi.arg(3));

        System.out.println("\t swipl.syntax = " +
                Query.oneSolution("jpl_pl_syntax(Syntax)").get("Syntax"));
        System.out.println("\t swipl.home = " +
                Query.oneSolution("current_prolog_flag(home,Home)").get("Home").name());
        System.out.println("\t jpl.jar = " + JPL.version_string());
        System.out.println("\t jpl.dll = " + org.jpl7.fli.Prolog.get_c_lib_version());
        System.out.println("\t jpl.pl = " + Query.oneSolution("jpl_pl_lib_version(V)").get("V").name());


        System.out.println("\t java.version = " + System.getProperty("java.version"));
        System.out.println("\t java_lib_version = " + JPL.version_string());
        System.out.println("\t c_lib_version = " + org.jpl7.fli.Prolog.get_c_lib_version());
        System.out.println("\t pl_lib_version = " +
                new Query(new Compound("jpl_pl_lib_version", new Term[] { new Variable("V") }))
                        .oneSolution().get("V"));
        System.out.println("\t os.name = " + System.getProperty("os.name"));
        System.out.println("\t os.arch = " + System.getProperty("os.arch"));
        System.out.println("\t os.version = " + System.getProperty("os.version"));

        System.out.println();
        report_actual_init_args();
        System.out.println();

    }

    private static void report_actual_init_args() {
        String[] args = org.jpl7.fli.Prolog.get_default_init_args();
        String which;
        String s = "";
        if (args == null) {
            args = org.jpl7.fli.Prolog.get_actual_init_args();
            which = "actual";
        } else {
            which = "default";
        }
        for (int i = 0; i < args.length; i++) {
            s = s + args[i] + " ";
        }
        System.out.println(String.format("\t %s_init_args = %s", which, s));
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


    // the tests; all public void test*()

    /**
     * TEST DISABLED!
     *
     * This test is meant to abort a query that is taking too long but Query.abort() does not exists!
     */
//    @Test
    public void testSlowGoal() {
        final Query q = new Query("sleep(10)"); // 10 successive sleep(1)
        Thread t = new Thread(new Runnable() {
            public void run() {
                try {
                    System.out.println("q.hasSolution() ... ");
                    System.out.println(q.hasSolution() ? "finished" : "failed");
                } catch (Exception e) {
                    System.out.println("q.hasSolution() threw " + e);
                }
            }
        });
        t.start(); // call the query in a separate thread
        System.out.println("pausing for 2 secs...");
        try {
            Thread.sleep(2000);
        } catch (InterruptedException e) {
            ;
        } // wait a coupla seconds for it to get started
        // (new Query("set_prolog_flag(abort_with_exception,
        // true)")).hasSolution();
        System.err.println("calling q.abort()...");
//        q.abort();
        System.err.println();
    }


    @Test
	public void testInfo() {
		Term swi = Query.oneSolution("current_prolog_flag(version_data,Swi)").get("Swi");
		System.out.println("\t version = " + swi.arg(1) + "." + swi.arg(2) + "." + swi.arg(3));
		System.out.println("\t syntax = " + Query.oneSolution("jpl:jpl_pl_syntax(Syntax)").get("Syntax"));
		System.out.println("\t jpl.jar = " + JPL.version_string() + " " + JPL.jarPath());
		System.out.println("\t jpl.dll = " + Prolog.get_c_lib_version());
		System.out.println("\t jpl.pl = " + Query.oneSolution("jpl:jpl_pl_lib_version(V)").get("V").name() + " "
				+ Query.oneSolution("module_property(jpl, file(F))").get("F").name());
		System.out.println("\t home = " + Query.oneSolution("current_prolog_flag(home,Home)").get("Home").name());
	}


    @Test
    public void testSyntaxSet1() {
        if (syntax.equals("traditional")) {
            try {
                JPL.setTraditional(); // should succeed silently
            } catch (Exception e) {
                fail("setTraditional() under traditional syntax threw exception: " + e);
            }
        } else {
            try {
                JPL.setTraditional();
            } catch (JPLException e) { // expected exception class, but is it
                // correct in detail?
                if (e.getMessage().endsWith("traditional syntax after Prolog is initialised")) {
                    // OK: an appropriate exception was thrown
                } else {
                    fail("setTraditional() under modern syntax threw incorrect JPLException: " + e);
                }
            } catch (Exception e) {
                fail("setTraditional() under modern syntax threw unexpected class of exception: " + e);
            }
        }
    }

    @Test
    public void testMasstest() {
        assertTrue((new Query("assert(diagnose_declaration(_,_,_,[not,a,real,error]))")).hasSolution());
    }

    @Test
    public void testSameLibVersions1() {
        String java_lib_version = JPL.version_string();
        String c_lib_version = Prolog.get_c_lib_version();
        assertTrue("java_lib_version(" + java_lib_version + ") is same as c_lib_version(" + c_lib_version + ")",
                java_lib_version.equals(c_lib_version));
    }

    @Test
    public void testSameLibVersions2() {
        String java_lib_version = JPL.version_string();
        String pl_lib_version = Query.oneSolution("jpl_pl_lib_version(V)").get("V").name();
        assertTrue("java_lib_version(" + java_lib_version + ") is same as pl_lib_version(" + pl_lib_version + ")",
                java_lib_version.equals(pl_lib_version));
    }




    @Test
    public void testPrologException1() {
        try {
            new Query("p(]"); // writes junk to stderr and enters debugger
            // unless flag debug_on_error = false
        } catch (PrologException e) {
            assertTrue("new Query(\"p(]\") throws a PrologException " + e.toString(), true);
            return;
        }
        fail("new Query(\"p(]\") oughta throw a PrologException");
    }




    // public void testMaxInteger1() {
    // assertEquals(Query.oneSolution("current_prolog_flag(max_integer,I)").get("I").longValue(),
    // java.lang.Long.MAX_VALUE); // i.e. 9223372036854775807L
    // }

    // public void testSingleton1() {
    // assertTrue(Query.hasSolution("style_check(-singleton),consult('test_singleton.pl')"));
    // }

	@Test
    public void testStaticQueryInvalidSourceText2() {
        String goal = "p(]";
        try {
            Query.hasSolution(goal); // should throw exception
            fail(goal + " (bad syntax) succeeded"); // shouldn't get to here
        } catch (PrologException e) { // expected exception
            if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("syntax_error", 1)
                    && e.term().arg(1).arg(1).hasFunctor("cannot_start_term", 0)) {
                // OK: an appropriate exception was thrown
            } else {
                fail(goal + " (bad syntax) threw wrong PrologException: " + e);
            }
        } catch (Exception e) {
            fail(goal + " (bad syntax) threw wrong exception class: " + e);
        }
    }

	@Test
    public void testStaticQueryInvalidSourceText1() {
        String goal = "bad goal";
        try {
            Query.hasSolution(goal); // should throw exception
            fail(goal + " (bad syntax) succeeded"); // shouldn't get to here
        } catch (PrologException e) { // expected exception
            if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("syntax_error", 1)
                    && e.term().arg(1).arg(1).hasFunctor("operator_expected", 0)) {
                // OK: an appropriate exception was thrown
            } else {
                fail(goal + " (bad syntax) threw wrong PrologException: " + e);
            }
        } catch (Exception e) {
            fail(goal + " (bad syntax) threw wrong exception class: " + e);
        }
    }

	@Test
    public void testStaticQueryNSolutions1() {
        String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
        int n = 5;
        assertTrue("Query.nSolutions(" + goal + ", " + n + ") returns " + n + " solutions",
                Query.nSolutions(goal, n).length == n);
    }

	@Test
    public void testStaticQueryNSolutions2() {
        String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
        int n = 0;
        assertTrue("Query.nSolutions(" + goal + ", " + n + ") returns " + n + " solutions",
                Query.nSolutions(goal, n).length == n);
    }

	@Test
    public void testStaticQueryNSolutions3() {
        String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
        int n = 20;
        assertTrue("Query.nSolutions(" + goal + ", " + n + ") returns 10 solutions",
                Query.nSolutions(goal, n).length == 10);
    }



	@Test
    public void testBerhhard1() {
        assertTrue(Query.allSolutions("consult(library('lists'))").length == 1);
    }

	@Test
    public void testWouter1() { // Wouter says this fails under OS X Mavericks
        // 10.9 x86-64
        long n = 7381783232223l; // too big for an int
        Compound term = new Compound("is", new Term[]{new Variable("X"), new Integer(n)});
        Map<String, Term>[] solutions = new Query(term).allSolutions();
        assertEquals(1, solutions.length);
        Map<String, Term> solution = solutions[0];
        assertTrue(solution.containsKey("X"));
        Object result = solution.get("X");
        assertTrue(result instanceof Integer);
        assertEquals(n, ((Integer) result).longValue());
    }


	@Test
    public void testForeignFrame1() {
        int ls1 = Query.oneSolution("statistics(localused,LS)").get("LS").intValue();
        int ls2 = Query.oneSolution("statistics(localused,LS)").get("LS").intValue();
        assertTrue("local stack size unchanged after query", ls1 == ls2);
    }

	@Test
    public void testOpenGetClose1() {
        StringBuffer sb = new StringBuffer();
        Query q = new Query("atom_chars(prolog, Cs), member(C, Cs)");
        Map<String, Term> soln;
        q.open();
        while (q.hasMoreSolutions()) {
            sb.append(((Atom) q.nextSolution().get("C")).name());
        }
        q.close();
        assertEquals("prolog", sb.toString());
    }

	@Test
    public void testOpenGetClose2() {
        Query q = new Query("dummy"); // we're not going to open this...
        try {
            q.nextSolution(); // should throw exception (query not open)
            fail("nextSolution() succeeds on unopened Query"); // shouldn't get
            // to here
        } catch (JPLException e) { // expected exception class
            if (e.getMessage().contains("existence_error")) {
                // OK: an appropriate exception was thrown
            } else {
                fail("jpl.Query#nextSolution() threw wrong JPLException: " + e);
            }
        } catch (Exception e) {
            fail("jpl.Query#nextSolution() threw wrong exception class: " + e);
        }
    }

	@Test
    public void testOpen1() {
        Query q = new Query("dummy");
        assertTrue("a newly created query is not open", !q.isOpen());
    }

	@Test
    public void testOpen2() {
        Query q = new Query("fail");
        q.open();
        assertTrue("a newly opened query which has no solutions is open", q.isOpen());
    }


}
