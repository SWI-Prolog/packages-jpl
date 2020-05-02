package org.jpl7.test.junit;

import org.jpl7.*;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.util.Map;

import static org.junit.Assert.*;

public class DataManagement extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.test.junit.DataManagement");

        // should work from static class but gives error
//        org.junit.runner.JUnitCore.main( GetSolution.class.getName()); // full name with package
    }

    /**
     * This is done at the class loading, before any test is run
     */
    @BeforeClass
    public static void setUp() {

        setUpClass();
        Query.hasSolution(String.format("consult('%s/test_quoted_module.pl')", test_dir)); // .pl file to be used
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
    public void quotedName1() {
        String text_t;
        String name;
        Term t;

//        name = "org.jpl7.PrologException: PrologException: error(existence_error(procedure, '/'(pepe, 1)), context(':'(system, '/'('$c_call_prolog', 0)), _0))";

        //name = "existence_error(procedure, '/'(pepe, 1))";
        //name = "'/'(pepe, 1)";
        name = "'$c_call_prolog'";
        t = Term.textToTerm(name);
        assert t != null;
        assertEquals("matching text-term", name, t.toString());
    }

    @Test
    public void errorTermTranslation1() {
        String text_t;
        String name;
        Term t;

        t = new Compound("error", new Term[] {
                new Atom("existence_error"),
                new Atom("context")});

        try {
            boolean x = Query.hasSolution(t);
        } catch (PrologException e) {
            assertTrue("PrologException has to be existence error", e.getMessage().contains("existence_error"));
        } catch (Exception e) {
            fail("Wrong exception type; should be PrologException but it is: " + e);
        }
    }


    private Term quotedQuery(String name) {
        return new Compound(":", new Term[]{
                new Atom("moduleTest"),
                new Compound("quoted_name", new Term[]{
                        new Atom(name),
                        new Variable("S")
                })
        });

    }

    @Test
    public void quotedName2() {
        String text_t;
        String name;
        Term t;
        Map<String, Term> sol;

        final String[] tests = { "hello", "hello(world)", "[1,2,3]" };
        final String[] expectedSolutions = { "hello", "\'hello(world)\'", "'[1,2,3]'" };


        int l = tests.length;
        for (int solutionIndex = 0; solutionIndex < l; solutionIndex++) {
//            t = quotedQuery(tests[solutionIndex]);
            sol = new Query(String.format("quoted_name(%s,S)", tests[solutionIndex])).oneSolution();

            //noinspection ConstantConditions
            System.out.println("\t The solution for S is: " + sol.get("S").toString());
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
        // System.out.println("type = " + t.typeName());
        assertEquals("foo", t.name());
        assertEquals(0, t.arity());
    }

    // public void testCompoundZeroArity3() {
    // Term t = Query.oneSolution("T = foo()").get("T");
    // assertTrue("term is a compound", t.isCompound());
    // assertFalse("term is an atom", t.isAtom());
    // }

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


}
