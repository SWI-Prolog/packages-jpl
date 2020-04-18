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

import java.util.Arrays;
import java.util.Map;

import static org.junit.Assert.*;


public class ListTest extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.test.junit.ListTest");

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
    public void testArrayToList1() {
        Term l2 = Util.termArrayToList(
                new Term[]{new Atom("a"), new Atom("b"), new Atom("c"), new Atom("d"), new Atom("e")});
        Query q9 = new Query(new Compound("append", new Term[]{new Variable("Xs"), new Variable("Ys"), l2}));
        assertTrue("append(Xs,Ys,[a,b,c,d,e]) has 6 solutions", q9.allSolutions().length == 6);
    }

    @Test
    public void testArrayToList2() {
        String goal = "append(Xs,Ys,[a,b,c,d,e])";
        assertTrue(goal + " has 6 solutions", Query.allSolutions(goal).length == 6);
    }

    @Test
    public void testArrayToList3() {
        final String[] expectedSolutions = { "a", "b", "c", "d", "e"};

        Term l2 = Util.termArrayToList(
                new Term[] { new Atom("a"), new Atom("b"), new Atom("c"), new Atom("d"), new Atom("e") });
        Query query = new Query(new Compound("member", new Term[] { new Variable("X"), l2 }));

        Map<String, Term>[] sol = query.allSolutions();
        for (int i = 0; i < sol.length; i++) {
            assertEquals(expectedSolutions[i], sol[i].get("X").toString());
        }
    }

    @Test
    public void testArrayToList4() {
        final String[] expectedSolutionsX = { "[]", "[a]", "[a, b]", "[a, b, c]"};
        final String[] expectedSolutionsY = { "[a, b, c]", "[b, c]", "[c]", "[]"};

        Term l2 = Util.termArrayToList(
                new Term[] { new Atom("a"), new Atom("b"), new Atom("c") });
        Query query = new Query(new Compound("append", new Term[] { new Variable("X"), new Variable("Y"), l2 }));

        Map<String, Term>[] sol = query.allSolutions();
        for (int i = 0; i < sol.length; i++) {

            String ListX = Arrays.toString(Util.atomListToStringArray(sol[i].get("X")));
            String ListY = Arrays.toString(Util.atomListToStringArray(sol[i].get("Y")));


            assertEquals("Bad X in append(X, Y, [a, b, c])", expectedSolutionsX[i], ListX);
            assertEquals("Bad Y in append(X, Y, [a, b, c])", expectedSolutionsY[i], ListY);

        }
    }







    @Test
    public void testLength1() {
        Query q5 = new Query(new Compound("length", new Term[]{new Variable("Zs"), new Integer(2)}));
        Term zs = q5.oneSolution().get("Zs");
        assertTrue("length(Zs,2) binds Zs to a list of two distinct variables " + zs.toString(),
                zs.isListPair() && zs.arg(1).isVariable() && zs.arg(2).isListPair() && zs.arg(2).arg(1).isVariable()
                        && zs.arg(2).arg(2).isListNil() && !zs.arg(1).name().equals(zs.arg(2).arg(1).name()));
    }

    @Test
    public void testListNil1() {
        Term x = Query.oneSolution("X = []").get("X");
        if (syntax.equals("traditional")) {
            assertTrue("empty list is text atom []",
                    x.isAtom() && x.atomType().equals("text") && x.name().equals("[]"));
        } else {
            assertTrue("empty list is reserved atom []",
                    x.isAtom() && x.atomType().equals("reserved_symbol") && x.name().equals("[]"));
        }
    }

    @Test
    public void testListCons1() {
        Term x = Query.oneSolution("X = [a]").get("X");
        if (syntax.equals("traditional")) {
            assertTrue("list constructor is ./2", x.isCompound() && x.name().equals("."));
        } else {
            assertTrue("list constructor is [|]/2", x.isCompound() && x.name().equals("[|]"));
        }
    }

    @Test
    public void testGenerate1() { // we chickened out of verifying each solution
        // :-)
        String goal = "append(Xs,Ys,[_,_,_,_,_])";
        assertTrue(goal + " has 6 solutions", Query.allSolutions(goal).length == 6);
    }


    // public void testFetchCyclicTerm(){
    // assertTrue((new Query("X=f(X)")).hasSolution());
    // }
    @Test
    public void testFetchLongList0() {
        assertTrue((new Query("findall(foo(N),between(0,10,N),L)")).hasSolution());
    }

    @Test
    public void testFetchLongList1() {
        assertTrue((new Query("findall(foo(N),between(0,100,N),L)")).hasSolution());
    }

    @Test
    public void testFetchLongList2() {
        assertTrue((new Query("findall(foo(N),between(0,500,N),L)")).hasSolution());
    }

    // public void testFetchLongList2c() { /* leads to stack overflow */
    //	assertTrue((new Query("findall(foo(N),between(0,1023,N),L)")).hasSolution());
    //}

    // public void testFetchLongList2a() {
    // assertTrue((new
    // Query("findall(foo(N),between(0,2000,N),L)")).hasSolution());
    // }
    // public void testFetchLongList2b() {
    // assertTrue((new
    // Query("findall(foo(N),between(0,3000,N),L)")).hasSolution());
    // }
    // public void testFetchLongList3() {
    // assertTrue((new
    // Query("findall(foo(N),between(0,10000,N),L)")).hasSolution());
    // }


}
