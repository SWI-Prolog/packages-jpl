package org.jpl7.junit;

import org.jpl7.*;
import org.jpl7.Integer;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.util.Arrays;
import java.util.Map;

import static org.junit.Assert.*;


public class Test_List extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.junit.Test_List");

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


    private static Term[] terms_pair_integers = new Term[]{new Integer(1), new Integer(2)};
    private static Term[] terms_integers = new Term[]{new Integer(1), new Integer(2), new Integer(3)};
    private static Term[] terms_atoms = new Term[]{new Atom("a"), new Atom("b"), new Atom("c")};

    // Several list terms of numbers
    private static Term list_empty = Term.textToTerm("[]");
    private static Term list_unit = Term.textToTerm("[1]");
    private static Term list_unit_complex = Term.textToTerm("[[1,2,3]]");
    private static Term list_simple = Term.textToTerm("[1,2,3]");
    private static Term list_complex = Term.textToTerm("[1, [1,2,3], 3]");

    private static Term[] lists_many = new Term[]{list_empty, list_unit, list_unit_complex, list_simple, list_complex};
    private static Term[] lists_many_noempty = new Term[]{list_unit, list_unit_complex, list_simple, list_complex};



    ///////////////////////////////////////////////////////////////////////////////
    // TESTS
    ///////////////////////////////////////////////////////////////////////////////


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
    public void testListNil2() {
        Term x;

        x = Query.oneSolution("X = []").get("X");
        assertTrue("term should be empty list", x.isListNil());
        assertTrue("Util.isList on empty list", Term.isList(x));
        assertFalse("term is not a ListPair", x.isListPair());

        x = Query.oneSolution("X = [1, 2, 3]").get("X");
        assertFalse("term should NOT be empty list", x.isListNil());
        assertTrue("Util.isList on non-empty list", Term.isList(x));
        assertTrue("term is not a ListPair", x.isListPair());
    }

    @Test
    public void testIsList() {
        final String[] options = { "[]", "[1]", "[1,2,3]", "[1, [a, b, c], 2]", "[[1,2,3]]"};

        Term x;
        for (String opt : options) {
            x = Query.oneSolution(String.format("X = %s", opt)).get("X");
            assertTrue("term should be empty list - Util", Term.isList(x));
            assertTrue("term should be empty list - Util", x.isList());
        }
    }



    @Test
    public void testIsPairList() {
        final String[] options = { "[]", "[1]", "[1,2,3]", "[1, [a, b, c], 2]", "[[1,2,3]]"};

        Term t;
        for (Term t2 : lists_many_noempty) {
            String msg = String.format("term %s should be a pair list", t2.toString());
            assertTrue(msg, t2.isListPair());
        }

        t = new Compound(JPL.LIST_PAIR, terms_pair_integers);
        assertTrue("term is a pair list (even though second arg is not a list)", t.isListPair());


        assertFalse("empty list term is not a list pair", JPL.LIST_NIL.isListPair());

        t = new Compound(JPL.LIST_PAIR, terms_integers);
        assertFalse("term is not a pair list, has more than two arguments", t.isListPair());

        t = new Compound("hello", terms_integers);
        assertFalse("term is not a pair list, not JPL.PAIR_LIST functor", t.isListPair());

    }

    @Test
    public void testIsPairList2() {
        Term t = new Compound(JPL.LIST_PAIR,
                new Term[]{new Integer(1), new Integer(2)});

        String msg = String.format("term %s should be a pair list", t.toString());
        assertTrue(msg, t.isListPair());

        assertEquals("[1, 2]", t.toString());
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
    public void testArrayToList1() {
        Term l = Term.termArrayToList(
                new Term[]{new Atom("a"), new Atom("b"), new Atom("c"),
                        new Atom("d"), new Atom("e")});
        Query q = new Query(new Compound("append",
                new Term[]{new Variable("Xs"), new Variable("Ys"), l}));

        assertEquals("append(Xs,Ys,[a,b,c,d,e]) has 6 solutions", 6, q.allSolutions().length);
    }


    @Test
    public void testArrayToList2() {
        final String[] expectedSolutions = { "a", "b", "c", "d", "e"};

        Term l = Term.termArrayToList(
                new Term[] { new Atom("a"), new Atom("b"), new Atom("c"),
                        new Atom("d"), new Atom("e") });
        Query query = new Query(new Compound("member",
                new Term[] { new Variable("X"), l }));

        Map<String, Term>[] sol = query.allSolutions();
        for (int i = 0; i < sol.length; i++) {
            assertEquals(expectedSolutions[i], sol[i].get("X").toString());
        }
    }

    @Test
    public void testArrayToList3() {
        final String[] expectedSolutionsX = { "[]", "[a]", "[a, b]", "[a, b, c]"};
        final String[] expectedSolutionsY = { "[a, b, c]", "[b, c]", "[c]", "[]"};

        Term l = Term.termArrayToList(
                new Term[] { new Atom("a"), new Atom("b"), new Atom("c") });
        Query q = new Query(new Compound("append",  // append(X, Y, [a, b, c])
                new Term[] { new Variable("X"), new Variable("Y"), l }));

        Map<String, Term>[] sol = q.allSolutions();
        for (int i = 0; i < sol.length; i++) {

            String ListX = Arrays.toString(Term.atomListToStringArray(sol[i].get("X")));
            String ListY = Arrays.toString(Term.atomListToStringArray(sol[i].get("Y")));


            assertEquals("Bad X in append(X, Y, [a, b, c])", expectedSolutionsX[i], ListX);
            assertEquals("Bad Y in append(X, Y, [a, b, c])", expectedSolutionsY[i], ListY);

        }
    }


    @Test
    public void testStringToList() {
        String goal = "append(Xs,Ys,[a,b,c,d,e])";
        assertEquals(goal + " has 6 solutions", 6, Query.allSolutions(goal).length);
    }





    @Test
    public void testLength1() {
        Query q5 = new Query(new Compound("length", new Term[]{new Variable("Zs"), new Integer(2)}));
        Term zs = q5.oneSolution().get("Zs");
        assertTrue("length(Zs,2) binds Zs to a list of two distinct variables " + zs.toString(),
                zs.isListPair() && zs.arg(1).isVariable() &&
                        zs.arg(2).isListPair() && zs.arg(2).arg(1).isVariable()
                        && zs.arg(2).arg(2).isListNil() &&
                        !zs.arg(1).name().equals(zs.arg(2).arg(1).name()));
    }

    @Test
    public void testGenerate1() { // we chickened out of verifying each solution
        // :-)
        String goal = "append(Xs,Ys,[_,_,_,_,_])";
        assertEquals(goal + " has 6 solutions", 6, Query.allSolutions(goal).length);
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


    @Test
    public void test_textToTerm_and_toString() {
        final String[] options = { "[]", "[1,2,3]", "[1]", "[1,g(2,3,5),[1,2,3],abc,[1],a,[],b]", "[[1,2,3]]" };
        final String[] options2 = { "[]", "[1, 2, 3]", "[1]", "[1, g(2, 3, 5), [1, 2, 3], abc, [1], a, [], b]", "[[1, 2, 3]]" };

        Term t;
        Term s;
        String msg;
        String opt, opt2;
        for (int i = 0; i < options.length; i++) {
            opt = options[i];
            msg = String.format("test Term.textToTerm on: %s", opt);

            t = Term.textToTerm(opt);
            s = Query.oneSolution(String.format("X = %s", opt)).get("X");
            assertTrue(msg, t.isList());
            assertEquals(t, s);

            opt2 = options2[i];
            msg = String.format("test Term.toString() on: %s", opt);
            assertEquals(msg, opt2, s.toString());

        }
    }


}
