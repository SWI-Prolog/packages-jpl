package org.jpl7.junit;

import java.util.Arrays;
import java.util.Map;
import org.jpl7.*;
import org.jpl7.Integer; // prefer to java.lang.Integer
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

public class Test_List extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.jpl7.JPLTestSuiteRunner.run(Test_List.class); // full name with package
    }

    private static Term[] terms_pair_integers = new Term[]{new Integer(1), new Integer(2)};
    private static Term[] terms_integers = new Term[]{new Integer(1), new Integer(2), new Integer(3)};
//  private static Term[] terms_atoms = new Term[]{new Atom("a"), new Atom("b"), new Atom("c")}; // not (yet) used
//  private static Term list_empty = Term.textToTerm("[]"); // not (yet) used
    private static Term list_unit = Term.textToTerm("[1]");
    private static Term list_unit_complex = Term.textToTerm("[[1,2,3]]");
    private static Term list_simple = Term.textToTerm("[1,2,3]");
    private static Term list_complex = Term.textToTerm("[1, [1,2,3], 3]");
//  private static Term[] lists_many = new Term[]{list_empty, list_unit, list_unit_complex, list_simple, list_complex}; // not (yet) used
    private static Term[] lists_many_noempty = new Term[]{list_unit, list_unit_complex, list_simple, list_complex};

    @Test
    public void testListNil1() {
        Term x = Query.oneSolution("X = []").get("X");
        if (syntax.equals("traditional")) {
            assertTrue(x.isAtom() && x.atomType().equals("text") && x.name().equals("[]"), "empty list is text atom []");
        } else {
            assertTrue(x.isAtom() && x.atomType().equals("reserved_symbol") && x.name().equals("[]"), "empty list is reserved atom []");
        }
    }

    @Test
    public void testListNil2() {
        Term x = Query.oneSolution("X = []").get("X");
        assertTrue(x.isListNil(), "term should be empty list");
        assertTrue(Term.isList(x), "Util.isList on empty list");
        assertFalse(x.isListPair(), "term is not a ListPair");
    }

    @Test
    public void testListNil3() {
        Term x = Query.oneSolution("X = [1, 2, 3]").get("X");
        assertFalse(x.isListNil(), "term should NOT be empty list");
        assertTrue(Term.isList(x), "Util.isList on non-empty list");
        assertTrue(x.isListPair(), "term is not a ListPair");
    }

    @Test
    public void testIsList() {
        final String[] options = { "[]", "[1]", "[1,2,3]", "[1, [a, b, c], 2]", "[[1,2,3]]"};
        for (String opt : options) {
        	Term x = Query.oneSolution(String.format("X = %s", opt)).get("X");
            assertTrue(Term.isList(x), "term should be empty list - Util");
            assertTrue(x.isList(), "term should be empty list - Util");
        }
    }

    @Test
    public void testIsPairList() {
        Term t;
        for (Term t2 : lists_many_noempty) {
            String msg = String.format("term %s should be a pair list", t2.toString());
            assertTrue(t2.isListPair(), msg);
        }
        t = new Compound(JPL.LIST_PAIR, terms_pair_integers);
        assertTrue(t.isListPair(), "term is a pair list (even though second arg is not a list)");
        assertFalse(JPL.LIST_NIL.isListPair(), "empty list term is not a list pair");
        t = new Compound(JPL.LIST_PAIR, terms_integers);
        assertFalse(t.isListPair(), "term is not a pair list, has more than two arguments");
        t = new Compound("hello", terms_integers);
        assertFalse(t.isListPair(), "term is not a pair list, not JPL.PAIR_LIST functor");
    }

    @Test
    public void testIsPairList2() {
        Term t = new Compound(JPL.LIST_PAIR,
                new Term[]{new Integer(1), new Integer(2)});
        String msg = String.format("term %s should be a pair list", t.toString());
        assertTrue(t.isListPair(), msg);
        assertEquals("[1, 2]", t.toString());
    }

    @Test
    public void testListCons1() {
        Term x = Query.oneSolution("X = [a]").get("X");
        if (syntax.equals("traditional")) {
            assertTrue(x.isCompound() && x.name().equals("."), "list constructor is ./2");
        } else {
            assertTrue(x.isCompound() && x.name().equals("[|]"), "list constructor is [|]/2");
        }
    }

    @Test
    public void testArrayToList1() {
        Term l = Term.termArrayToList(
                new Term[]{new Atom("a"), new Atom("b"), new Atom("c"),
                        new Atom("d"), new Atom("e")});
        Query q = new Query(new Compound("append",
                new Term[]{new Variable("Xs"), new Variable("Ys"), l}));
        assertEquals(6, q.allSolutions().length, "append(Xs,Ys,[a,b,c,d,e]) has 6 solutions");
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
            assertEquals(expectedSolutionsX[i], ListX, "Bad X in append(X, Y, [a, b, c])");
            assertEquals(expectedSolutionsY[i], ListY, "Bad Y in append(X, Y, [a, b, c])");
        }
    }

    @Test
    public void testStringToList() {
        String goal = "append(Xs,Ys,[a,b,c,d,e])";
        assertEquals(6, Query.allSolutions(goal).length, goal + " has 6 solutions");
    }

    @Test
    public void testLength1() {
        Query q5 = new Query(new Compound("length", new Term[]{new Variable("Zs"), new Integer(2)}));
        Term zs = q5.oneSolution().get("Zs");
        assertTrue(zs.isListPair() && zs.arg(1).isVariable() &&
                        zs.arg(2).isListPair() && zs.arg(2).arg(1).isVariable()
                        && zs.arg(2).arg(2).isListNil() &&
                        !zs.arg(1).name().equals(zs.arg(2).arg(1).name()), "length(Zs,2) binds Zs to a list of two distinct variables " + zs.toString());
    }

    @Test
    public void testGenerate1() {
        String goal = "append(Xs,Ys,[_,_,_,_,_])";
        assertEquals(6, Query.allSolutions(goal).length, goal + " has 6 solutions");
    }

    @Disabled
    @Test
    public void testFetchCyclicTerm(){
        assertTrue((new Query("X=f(X)")).hasSolution()); // causes a stack overflow in JVM :-/
    }
    
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

    @Test
    public void testFetchLongList2c() {
    	assertTrue((new Query("findall(foo(N),between(0,1000,N),L)")).hasSolution());
    }

    @Test
    public void testFetchLongList2a() {
        assertTrue((new Query("findall(foo(N),between(0,2000,N),L)")).hasSolution());
    }
    
    @Test
    public void testFetchLongList2b() {
        assertTrue((new Query("findall(foo(N),between(0,3000,N),L)")).hasSolution());
    }
    
    @Test
    public void testFetchLongList3() {
        assertTrue((new Query("findall(foo(N),between(0,10000,N),L)")).hasSolution());
    }

    @Test
    public void testPutShortList1() {
    	final int nA = 50;
    	int[] a = new int[nA];
    	Term t = Term.intArrayToList(a);
    	Map<String, Term> soln = Query.oneSolution("length(?,Nb)", new Term[] {t});
    	assertEquals(nA, soln.get("Nb").intValue(), "list of " + nA + " integers has expected length");
    }

    @Test
    public void testPutLongList1() {
    	final int nA = 5000;
    	int[] a = new int[nA];
    	Term t = Term.intArrayToList(a);
    	Map<String, Term> soln = Query.oneSolution("length(?,Nb)", new Term[] {t});
    	assertEquals(nA, soln.get("Nb").intValue(), "list of " + nA + " integers has expected length");
    }

    @Test
    public void test_textToTerm_and_toString() {
        final String[] options = { "[]", "[1,2,3]", "[1]", "[1,g(2,3,5),[1,2,3],abc,[1],a,[],b]", "[[1,2,3]]" };
        final String[] options2 = { "[]", "[1, 2, 3]", "[1]", "[1, g(2, 3, 5), [1, 2, 3], abc, [1], a, [], b]", "[[1, 2, 3]]" };
        for (int i = 0; i < options.length; i++) {
        	String opt = options[i];
        	String msg = String.format("test Term.textToTerm on: %s", opt);
        	Term t = Term.textToTerm(opt);
        	Term s = Query.oneSolution(String.format("X = %s", opt)).get("X");
            assertTrue(t.isList(), msg);
            assertEquals(t, s);
            String opt2 = options2[i];
            String msg2 = String.format("test Term.toString() on: %s", opt);
            assertEquals(opt2, s.toString(), msg2);
        }
    }
}
