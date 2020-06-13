package org.jpl7.junit;

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

import java.util.Map;

import static org.junit.Assert.*;

// This class defines all the tests which are run from Java.
public class Tests extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.junit.Tests");

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


    ///////////////////////////////////////////////////////////////////////////////
    // SUPPORTING CODE
    ///////////////////////////////////////////////////////////////////////////////



    ///////////////////////////////////////////////////////////////////////////////
    // TESTS
    ///////////////////////////////////////////////////////////////////////////////

    @Test
    public void testSameLibVersions1() {
        String java_lib_version = JPL.version_string();
        String c_lib_version = Prolog.get_c_lib_version();

        String msg = String.format("java_lib_version(%s) is same as c_lib_version(%s)",  java_lib_version, c_lib_version);
        assertEquals(msg, java_lib_version, c_lib_version);
    }

//    @Test
    public void testSameLibVersions2() {
        String java_lib_version = JPL.version_string();

        useJPLmodule();
        String pl_lib_version = Query.oneSolution("jpl_pl_lib_version(V)").get("V").name();

        String msg = String.format("java_lib_version(%s) is same as pl_lib_version(%s)", java_lib_version, pl_lib_version);
        assertEquals(msg, java_lib_version, pl_lib_version);
    }


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
        } catch (InterruptedException ignored) {
            ;
        } // wait a coupla seconds for it to get started
        // (new Query("set_prolog_flag(abort_with_exception,
        // true)")).hasSolution();
        System.err.println("calling q.abort()...");
//        q.abort();
        System.err.println();
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


    @Test
    public void testSingleton1() {
        assertTrue(Query.hasSolution(
                String.format("style_check(-singleton), consult('%s/test_singleton.pl')", test_dir)));
    }

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
        assertEquals("Query.nSolutions(" + goal + ", " + n + ") returns " + n + " solutions", Query.nSolutions(goal, n).length, n);
    }

	@Test
    public void testStaticQueryNSolutions2() {
        String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
        int n = 0;
        assertEquals("Query.nSolutions(" + goal + ", " + n + ") returns " + n + " solutions", Query.nSolutions(goal, n).length, n);
    }

	@Test
    public void testStaticQueryNSolutions3() {
        String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
        int n = 20;
        assertEquals("Query.nSolutions(" + goal + ", " + n + ") returns 10 solutions", 10, Query.nSolutions(goal, n).length);
    }



	@Test
    public void testBerhhard1() {
        assertEquals(1, Query.allSolutions("consult(library('lists'))").length);
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
        assertEquals("local stack size unchanged after query", ls1, ls2);
    }



	@Test
    public void testOpen1() {
        Query q = new Query("dummy");
        assertFalse("a newly created query is not open", q.isOpen());
    }

	@Test
    public void testOpen2() {
        Query q = new Query("fail");
        q.open();
        assertTrue("a newly opened query which has no solutions is open", q.isOpen());
    }


}
