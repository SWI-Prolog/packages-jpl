package org.jpl7.test.junit;

import org.jpl7.*;

import java.util.Map;
import java.util.NoSuchElementException;

import org.jpl7.Integer;
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
        assertFalse("A query has exhausted all solutions but it is still open", q.isOpen());
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
        assertFalse(q.isOpen()); // q will have been closed by solution exhaustion
        assertEquals("rolo", sb.toString());
    }






    @Test
    public void testStaticQueryAllSolutions1() {
        String goal = "member(X, [0,1,2,3,4,5,6,7,8,9])";
        assertEquals("Query.allSolutions(" + goal + ") returns 10 solutions", 10, Query.allSolutions(goal).length);
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
        assertEquals("in dont-tell-me mode, setof(_M,current_module(_M),_Ms),length(_Ms,N) returns binding for just one variable", 1, q.oneSolution().keySet().size());
    }

    @Test
    public void testDontTellMeMode2() {
        final Query q = new Query("setof(_M,current_module(_M),_Ms),length(_Ms,N)");
        JPL.setDTMMode(false);
        assertEquals("not in dont-tell-me mode, setof(_M,current_module(_M),_Ms),length(_Ms,N) returns binding for three variables", 3, q.oneSolution().keySet().size());
    }

    @Test
    public void testMap1() {
        Map<String, Term> h = Query.oneSolution("p(a,b) = p(X,Y)");
        assertEquals("a", h.get("X").name());
        assertEquals("b", h.get("Y").name());
    }

    @Test
    public void testMap2() {
        Map<String, Term>[] hs = Query.allSolutions("p(a,b) = p(X,Y)");
        assertEquals(1, hs.length);
        assertEquals("a", hs[0].get("X").name());
        assertEquals("b", hs[0].get("Y").name());
    }


}
