package org.jpl7.test.junit;

import org.jpl7.*;

import java.util.Map;
import java.util.NoSuchElementException;

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

// In case we want to use advanced loggers in the future
//import org.slf4j.Logger;
//import org.slf4j.LoggerFactory;

public class GetSolution extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.test.junit.GetSolution");

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
    public void testGetSolution1() {
        Query q = new Query("fail");
        q.open();
        if (q.hasMoreSolutions()) q.nextSolution();
        assertTrue("A query has exhausted all solutions but it is still open", !q.isOpen());
    }

    @Test
    public void testGetSolution2() {
        Query q = new Query("fail"); // this query has no solutions
        q.open(); // this opens the query
        try {
            q.nextSolution(); // this call is invalid, as the query is closed

            // shouldn't get to here
            fail("jpl.Query#nextSolution() should have thrown JPLException");
        } catch (NoSuchElementException e) {
            // all good, right exception threw
        } catch (Exception e) {
            fail("jpl.Query#nextSolution() threw wrong class of exception: " + e);
        }
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
    public void testStackedQueries1() {
        StringBuffer sb = new StringBuffer();
        Query q = new Query("atom_chars(prolog, Cs), member(C, Cs)");
        Map<String, Term> soln;
        q.open();
        while (q.hasMoreSolutions()) {
            soln = q.nextSolution();
            Atom a = (Atom) soln.get("C");
            if (Query.hasSolution("memberchk(?, [l,o,r])", new Term[] { a })) {
                // this query opens and closes while an earlier query is still open
                sb.append(((Atom) soln.get("C")).name());
            }
        }
        assertTrue(!q.isOpen()); // q will have been closed by solution exhaustion
        assertEquals("rolo", sb.toString());
    }


    @Test
    public void iterativeSol1() {
        final int[] expectedSolutions = { 1, 2, 3, 4, 5};

        Query q = new Query("between(1,5,N)");
        Map<String,Term> sol;
        int solutionIndex;

        solutionIndex = 0;
        while (q.hasMoreSolutions()) {
            sol = q.nextSolution();

            assertEquals(expectedSolutions[solutionIndex], sol.get("N").intValue());
            solutionIndex++;
        }

        assertFalse("There should not be more solutions for the query", q.hasSolution());
        assertFalse("Query should be closed due to sol exhaustion", q.isOpen());

        q.reset();
        assertFalse("Query is still close after reset", q.isOpen());
        assertTrue("Query again should have solutions after reset", q.hasSolution());

        solutionIndex = 0;
        while (q.hasMoreSolutions()) {
            sol = q.nextSolution();

            assertEquals(expectedSolutions[solutionIndex], sol.get("N").intValue());
            solutionIndex++;
        }
        assertFalse("There should not be more solutions for the query", q.hasSolution());
        assertFalse("Query should be closed due to sol exhaustion", q.isOpen());

        try {
            sol = q.next();
            fail("Query has been exhausted, there should be no solutions left");

        } catch (NoSuchElementException e) {
            // all good, this is what we expect as there are no more solutions
        } catch (Exception e) {
            fail("Wrong exception type: "  + e);
        }

    }




    @Test
    public void testStaticQueryAllSolutions1() {
        String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
        assertTrue("Query.allSolutions(" + goal + ") returns 10 solutions", Query.allSolutions(goal).length == 10);
    }

    @Test
    public void testStaticQueryHasSolution1() {
        String goal = "memberchk(13, [?,?,?])";
        Term[] params = new Term[]{new Integer(12), new Integer(13), new Integer(14)};
        assertTrue(Query.hasSolution(goal, params));
    }

    @Test
    public void testStaticQueryHasSolution2() {
        String goal = "memberchk(23, [?,?,?])";
        Term[] params = new Term[]{new Integer(12), new Integer(13), new Integer(14)};
        assertFalse(Query.hasSolution(goal, params));
    }

    @Test
    public void testDontTellMeMode1() {
        final Query q = new Query("setof(_M,current_module(_M),_Ms),length(_Ms,N)");
        JPL.setDTMMode(true);
        assertTrue(
                "in dont-tell-me mode, setof(_M,current_module(_M),_Ms),length(_Ms,N) returns binding for just one variable",
                q.oneSolution().keySet().size() == 1);
    }

    @Test
    public void testDontTellMeMode2() {
        final Query q = new Query("setof(_M,current_module(_M),_Ms),length(_Ms,N)");
        JPL.setDTMMode(false);
        assertTrue(
                "not in dont-tell-me mode, setof(_M,current_module(_M),_Ms),length(_Ms,N) returns binding for three variables",
                q.oneSolution().keySet().size() == 3);
    }

    @Test
    public void testMap1() {
        Map<String, Term> h = Query.oneSolution("p(a,b) = p(X,Y)");
        assertTrue(h.get("X").name().equals("a"));
        assertTrue(h.get("Y").name().equals("b"));
    }

    @Test
    public void testMap2() {
        Map<String, Term>[] hs = Query.allSolutions("p(a,b) = p(X,Y)");
        assertTrue(hs.length == 1);
        assertTrue(hs[0].get("X").name().equals("a"));
        assertTrue(hs[0].get("Y").name().equals("b"));
    }


}
