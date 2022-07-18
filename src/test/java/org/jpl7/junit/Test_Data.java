package org.jpl7.junit;

import java.util.Map;
import org.jpl7.*;
import static org.junit.Assert.*;
import org.junit.BeforeClass;
import org.junit.Test;

public class Test_Data extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main(Test_Data.class.getName()); // full name with package
    }

    @BeforeClass
    public static void setUp() {
        setUpClass();
        Query.hasSolution(String.format("consult('%s/test_quoted_module.pl')", test_dir)); // .pl file to be used
    }

    @Test
    public void quotedName1() {
        String name = "'$c_call_prolog'";
        Term t = Term.textToTerm(name);
        assert t != null;
        assertEquals("matching text-term", name, t.toString());
    }

    @Test
    public void errorTermTranslation1() {
        Term t = new Compound("error", new Term[] {
                new Atom("existence_error"),
                new Atom("context")});
        try {
            Query.hasSolution(t);
        } catch (PrologException e) {
            assertTrue("PrologException has to be existence error", e.getMessage().contains("existence_error"));
        } catch (Exception e) {
            fail("Wrong exception type; should be PrologException but it is: " + e);
        }
    }

    @Test
    public void quotedName2() {
        final String[] tests = { "hello", "hello(world)", "[1,2,3]" };
        final String[] expectedSolutions = { "hello", "\'hello(world)\'", "'[1,2,3]'" };
        int l = tests.length;
        for (int solutionIndex = 0; solutionIndex < l; solutionIndex++) {
        	Map<String, Term> sol = new Query(String.format("quoted_name(%s,S)", tests[solutionIndex])).oneSolution();
            //noinspection ConstantConditions
            reportNoise("\t The solution for S is: " + sol.get("S").toString());
            assertEquals(expectedSolutions[solutionIndex], sol.get("S").toString());
        }
    }

    @Test
    public void testEmptyParentheses() {
        Term t = Query.oneSolution("T = a()").get("T"); // valid in both
        // traditional and
        // modern syntax in SWI
        // Prolog 7.x
        assertTrue("T is not bound to an atom", t.isAtom());
        assertEquals("the atom's name is not \"a\"", "a", t.name());
    }

    @Test
    public void testCompoundZeroArity1() {
        Term t = new Compound("foo", new Term[]{});
        assertTrue(t.isCompound());
        assertFalse(t.isAtom());
        assertEquals("foo", t.name());
        assertEquals(0, t.arity());
    }

    @Test
    public void testCompoundZeroArity2() {
        Term t = Query.oneSolution("T = foo()").get("T");
        assertEquals("foo", t.name());
        assertEquals(0, t.arity());
    }

    @Test
    public void testUtilListToTermArray1() {
        String goal = "T = [a,b,c]";
        Term list = Query.oneSolution(goal).get("T");
        Term[] array = Term.listToTermArray(list);
        assertTrue(array[2].isAtom() && array[2].name().equals("c"));
    }

    @Test
    public void testTermToTermArray1() {
        String goal = "T = [a,b,c]";
        Term list = Query.oneSolution(goal).get("T");
        Term[] array = list.listToTermArray();
        assertTrue(array[2].isAtom() && array[2].name().equals("c"));
    }

    @Test
    public void testTextToTerm1() {
        String text = "fred(B,p(A))";
        Term t = Term.textToTerm(text);
        assertTrue("Term.textToTerm() converts \"fred(B,p(A))\" to a corresponding Term",
                t.hasFunctor("fred", 2) && t.arg(1).isVariable() && t.arg(1).name().equals("B")
                        && t.arg(2).hasFunctor("p", 1) && t.arg(2).arg(1).isVariable()
                        && t.arg(2).arg(1).name().equals("A"));
    }

    @Test
    public void testTextToTerm2() {
        String text1 = "fred(?,2,?)";
        String text2 = "[first(x,y),A]";
        Term plist = Term.textToTerm(text2);
        @SuppressWarnings("ConstantConditions") Term[] ps = plist.listToTermArray();
        //noinspection ConstantConditions
        Term t = Term.textToTerm(text1).putParams(ps);
        assertTrue("fred(?,2,?) .putParams( [first(x,y),A] )",
                t.hasFunctor("fred", 3) && t.arg(1).hasFunctor("first", 2) && t.arg(1).arg(1).hasFunctor("x", 0)
                        && t.arg(1).arg(2).hasFunctor("y", 0) && t.arg(2).hasFunctor(2, 0) && t.arg(3).isVariable()
                        && t.arg(3).name().equals("A"));
    }

    // issue #13: https://github.com/SWI-Prolog/packages-jpl/issues/13
    @Test
    public void testWeirdCompound() {
        Term t = new Query("A = 'age(mary)'(1,2,3)").oneSolution().get("A");
        assertEquals("'age(mary)'(1, 2, 3)", t.toString());
    }

    // issue #13: https://github.com/SWI-Prolog/packages-jpl/issues/13
    @Test
    public void testWeirdCompound2() {
    	Map<String, Term> sol = new Query("A = '[1,2,3]'(a,b,c), A =.. [B1|B2]").oneSolution();
        assertEquals("'[1,2,3]'(a, b, c)", sol.get("A").toString());
        assertEquals("'[1,2,3]'", sol.get("B1").toString());
        assertEquals("[a, b, c]", sol.get("B2").toString());
    }
}
