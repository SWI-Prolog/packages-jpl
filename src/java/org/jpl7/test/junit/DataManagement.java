package org.jpl7.test.junit;

import org.jpl7.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.util.Map;

import static org.junit.Assert.*;

public class DataManagement {
    public static final String startup = (System.getenv("SWIPL_BOOT_FILE") == null ? "../../src/swipl.prc"
            : System.getenv("SWIPL_BOOT_FILE"));
    public static final String test_jpl = (System.getenv("TEST_JPL") == null ? "test_jpl.pl"
            : System.getenv("TEST_JPL"));
    public static final String syntax = (System.getenv("SWIPL_SYNTAX") == null ? "modern"
            : System.getenv("SWIPL_SYNTAX"));
    public static final String home = (System.getenv("SWI_HOME_DIR") == null ? "../.."
            : System.getenv("SWI_HOME_DIR"));


    public static void main(String argv[]) {
    }



    ///////////////////////////////////////////////////////////////////////////////
    // TESTING CONFIGURATION
    ///////////////////////////////////////////////////////////////////////////////
    @BeforeClass
    public static void setUp() {
	    assertTrue("quoted_name.pl should loaded for this test",
                Query.hasSolution("consult('src/java/org/jpl7/test/junit/quoted_name.pl')"));
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
    public void quotedName1() {
        String text_t;
        String name;
        Term t;

//        name = "org.jpl7.PrologException: PrologException: error(existence_error(procedure, '/'(pepe, 1)), context(':'(system, '/'('$c_call_prolog', 0)), _0))";
        name = "existence_error(procedure, '/'(pepe, 1))";
//        name = "'/'(pepe, 1)";
        name = "'$c_call_prolog'";
        t = Util.textToTerm(name);
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
                new Atom("jpl"),
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

            System.out.println("\t The solution for S is: " + sol.get("S").toString());
            assertEquals(expectedSolutions[solutionIndex], sol.get("S").toString());
        }



//        String name2 = ((Atom) sol.get("S")).name();

//        t = Util.textToTerm(String.format("jpl:(quoted_name(%s, S))", name));

    }


    @Test
    public void testEmptyParentheses() {
        Term t = Query.oneSolution("T = a()").get("T"); // valid in both
        // traditional and
        // modern syntax in SWI
        // Prolog 7.x
        assertTrue("T is not bound to an atom", t.isAtom());
        assertTrue("the atom's name is not \"a\"", t.name().equals("a"));
    }




    @Test
    public void testCompoundZeroArity1() {
        Term t = new Compound("foo", new Term[]{});
        assertTrue(t.isCompound());
        assertFalse(t.isAtom());
        assertTrue(t.name().equals("foo"));
        assertTrue(t.arity() == 0);
    }

    @Test
    public void testCompoundZeroArity2() {
        Term t = Query.oneSolution("T = foo()").get("T");
        // System.out.println("type = " + t.typeName());
        assertTrue(t.name().equals("foo"));
        assertTrue(t.arity() == 0);
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
        Term[] array = Util.listToTermArray(list);
        assertTrue(array[2].isAtom() && array[2].name().equals("c"));
    }

    @Test
    public void testTermToTermArray1() {
        String goal = "T = [a,b,c]";
        Term list = Query.oneSolution(goal).get("T");
        Term[] array = list.toTermArray();
        assertTrue(array[2].isAtom() && array[2].name().equals("c"));
    }



    @Test
    public void testTextToTerm1() {
        String text = "fred(B,p(A))";
        Term t = Util.textToTerm(text);
        assertTrue("Util.textToTerm() converts \"fred(B,p(A))\" to a corresponding Term",
                t.hasFunctor("fred", 2) && t.arg(1).isVariable() && t.arg(1).name().equals("B")
                        && t.arg(2).hasFunctor("p", 1) && t.arg(2).arg(1).isVariable()
                        && t.arg(2).arg(1).name().equals("A"));
    }



    @Test
    public void testTextToTerm2() {
        String text1 = "fred(?,2,?)";
        String text2 = "[first(x,y),A]";
        Term plist = Util.textToTerm(text2);
        Term[] ps = plist.toTermArray();
        Term t = Util.textToTerm(text1).putParams(ps);
        assertTrue("fred(?,2,?) .putParams( [first(x,y),A] )",
                t.hasFunctor("fred", 3) && t.arg(1).hasFunctor("first", 2) && t.arg(1).arg(1).hasFunctor("x", 0)
                        && t.arg(1).arg(2).hasFunctor("y", 0) && t.arg(2).hasFunctor(2, 0) && t.arg(3).isVariable()
                        && t.arg(3).name().equals("A"));
    }


}
