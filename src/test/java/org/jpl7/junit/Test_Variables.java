package org.jpl7.junit;

import org.jpl7.*;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.*;

// This is the original testbed ported to  JUNIT Version 4:
// https://junit.org/junit4/faq.html
//https://objectcomputing.com/resources/publications/sett/august-2007-migrating-from-junit-3-to-junit-4-nothing-but-good-news

// This class defines all the tests which are run from Java.
public class Test_Variables extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.junit.Test_Variables");

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
    public void test_variable_binding1() {
        Term lhs = new Compound("p", new Term[]{new Variable("X"), new Variable("Y")}); // p(X,Y)
        Term rhs = new Compound("p", new Term[]{new Atom("a"), new Atom("b")}); // p(a,b)
        Term goal = new Compound("=", new Term[]{lhs, rhs}); // p(X,Y) = p(a,b)

        Map<String, Term> soln = new Query(goal).oneSolution();
        assertTrue("two Variables with different names can bind to distinct atoms",
                soln != null && (soln.get("X")).name().equals("a") && (soln.get("Y")).name().equals("b"));
    }


    @Test
    public void test_variable_binding2() {
        Term lhs = new Compound("p", new Term[]{new Variable("X"), new Variable("X")});
        Term rhs = new Compound("p", new Term[]{new Atom("a"), new Atom("b")});
        Term goal = new Compound("=", new Term[]{lhs, rhs});
        assertFalse("two distinct Variables with same name cannot unify with distinct atoms",
                new Query(goal).hasSolution());
    }

    @Test
    public void test_variable_binding3() {
        Variable X = new Variable("X");
        Term lhs = new Compound("p", new Term[]{X, X});
        Term rhs = new Compound("p", new Term[]{new Atom("a"), new Atom("b")});
        Term goal = new Compound("=", new Term[]{lhs, rhs});
        assertFalse("two references to the same (named) Variable cannot unify with differing atoms",
                new Query(goal).hasSolution());
    }

    @Test
    public void test_variable_binding4() {
        Term lhs = new Compound("p", new Term[]{new Variable("_"), new Variable("_")});
        Term rhs = new Compound("p", new Term[]{new Atom("a"), new Atom("b")});
        Term goal = new Compound("=", new Term[]{lhs, rhs});
        assertTrue("two distinct anonymous Variables can unify with distinct atoms", new Query(goal).hasSolution());
    }

    @Test
    public void test_variable_binding5() {
        Variable Anon = new Variable("_");
        Term lhs = new Compound("p", new Term[]{Anon, Anon});
        Term rhs = new Compound("p", new Term[]{new Atom("a"), new Atom("b")});
        Term goal = new Compound("=", new Term[]{lhs, rhs});
        assertTrue("two references to an anonymous Variable can unify with differing atoms",
                new Query(goal).hasSolution());
    }

    Variable v = new Variable("X");

    @Test
    public void test_variable_binding6() {
        Variable v = new Variable("X");

        Term t1 = new Query("? = 5", v).oneSolution().get("X");
        assertEquals("Variable X must be bound to JPL Integer 5", 5, t1.intValue());

        // Variable v is re-used for another query, no problem
        Term t2 = new Query("? = 10", v).oneSolution().get("X");
        assertEquals("Variable X must be bound to JPL Integer 10", 10, t2.intValue());
    }


    @Test
    public void test_free_variable1() {
        Query q = new Query("append([X],[_,_],[_,_,_])");
        Map<String, Term> sol = q.oneSolution();

        assertEquals("Variable X must remain free after query",
                sol.get("X").typeName(), "Variable");
    }

    /**
     * TEST DISABLED!!
     * <p>
     * Gives error:
     * java.lang.AssertionError: Variable X must be bound to Y
     * Expected :Y
     * Actual   :_142
     */
//    @Test
    public void test_free_variable3() {
        Variable VarX = new Variable("X");
        Variable VarY = new Variable("Y");

//        Query q = new Query("append([X],[_,_],[_,_,_])");
        Query q = new Query(new Compound("=", new Term[]{VarX, VarY}));

        Map<String, Term> sol = q.oneSolution();

        assertEquals("Variable X must be bound to Y", VarY, sol.get("X"));
    }


    ////////////////////////////////////////////////////
    //// EQUALITY
    ////////////////////////////////////////////////////

    @Test
    public void test_variable_equal1() {
        Variable v1 = new Variable("X");
        Variable v2 = new Variable("X");

        Term s1 = new Query("? = 5", v1).oneSolution().get("X");
        Term s2 = new Query("? = 15", v2).oneSolution().get("X");

        assertTrue("Variables are equal textually despite different bindings", v1.equals(v2));   // evaluates to True
        assertFalse("Bindings are the same", s1.equals(s2)); // evaluates to False
    }

    @Test
    public void test_variable_equal2() {
        Term q1 = Term.textToTerm("X = 5");
        Term q2 = Term.textToTerm("X = 15");

        Variable v1 = (Variable) q1.arg(1);
        Variable v2 = (Variable) q2.arg(1);

        assertEquals("name of var is X", "X", v1.name());

        assertNotEquals("should be different variables, despite = names", v1.equals(v2));
    }


    @Test
    public void test_variable_hash1() {
        Term v1 = new Variable("X");
        Term v2 = new Variable("X");
        Term v3 = new Variable();

        Set<Term> boxes = new HashSet<>();
        boxes.add(v1);

        assertTrue("v2 should not be contained in set (<> hash)", boxes.contains(v2));
        assertFalse("v3 should not be contained in set (<> hash)", boxes.contains(v3));
    }


    @Test
    public void test_variable_equal_anonymous1() {
        Term v1 = new Variable();
        Term v2 = new Variable("X");
        Term v3 = new Variable("_X");


        assertTrue("anonymous var is equal to itself", v1.equals(v1));
        assertTrue("dont-tell var is equal to itself", v1.equals(v1));

        assertFalse("anonymous var is not equal to any var", v1.equals(v2) || v1.equals(v3));
    }

    @Test
    public void test_variable_equal_anonymous2() {
        Term t = new Query("length(X, 2)").oneSolution().get("X"); // X = [_13, _14]

        Term v1 = t.listToTermArray()[0];
        Term v2 = t.listToTermArray()[1];

        assertNotEquals(v1, v2);
    }


    @Test
    public void test_variable_equal_anonymous3() {
        Term t = new Query("copy_term([A,A,A], X).").oneSolution().get("X"); // X = [_12, _12, _12]

        Term v1 = t.listToTermArray()[0];
        Term v2 = t.listToTermArray()[1];

        assertEquals(v1, v2);
    }

    @Test
    public void test_variable_equal_anonymous4() {
        Term t = Term.textToTerm("related(X, _, _) = related(_, A, B)");

        Map<String, Term> sol = new Query(t).oneSolution();

        Term vA = sol.get("A");
        Term vB = sol.get("B");

        assertNotEquals(vA, vB);
    }

    @Test
    public void test_variable_equal_anonymous5() {
        Term t = Term.textToTerm("related(X, _Y, _Y) = related(_, A, B)");

        Map<String, Term> sol = new Query(t).oneSolution();

        Term vA = sol.get("A");
        Term vB = sol.get("B");

        assertEquals(vA, vB);
    }


    @Test
    public void test_variable_equal_anonymous6() {
        Variable v = new Variable("_");
        Variable v1 = new Variable();

        assertNotEquals("The anonymous var _ is not equal to itself", v, v);
        assertNotEquals("The anonymous var _ is not equal to dont tell vars", v, v1);
    }

    @Test
    public void test_variable_equal_anonymous7() {
        Term t = new Query("X = p(_), Y = q(X,X)").oneSolution().get("Y");

        Term t1 = t.arg(1); // t1 = p(_38)
        Term t2 = t.arg(2); // t2 = p(_38)

        assertEquals(t1, t2);

        Variable v1 = (Variable) t1.arg(1); // v1 = _38
        Variable v2 = (Variable) t2.arg(1); // v2 = _38

        assertEquals(v1, v2);

    }

}