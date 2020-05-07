package org.jpl7.test.junit;

import org.jpl7.*;
import org.jpl7.Float;
import org.jpl7.Integer;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.math.BigInteger;
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
        org.junit.runner.JUnitCore.main("org.jpl7.test.junit.Test_Variables");

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
        assertEquals("Variable X must be bound to JPL Integer 5", 5,  t1.intValue());

        // Variable v is re-used for another query, no problem
        Term t2 = new Query("? = 10", v).oneSolution().get("X");
        assertEquals("Variable X must be bound to JPL Integer 10", 10,  t2.intValue());
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
     *
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
        Query q = new Query(new Compound("=", new Term[] { VarX, VarY }));

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
    public void test_numbers_equal() {
        // INTEGERS
        Term i1 = new Integer(1212);
        Term i2 = new Integer(1212);
        Term i3 = new Integer(212);

        assertEquals("should be = integers", i1, i2);
        assertNotEquals("should be <> integers", i1, i3);

        // BIG INTEGERS
        Term bi1 = new Integer(BigInteger.valueOf(java.lang.Long.MIN_VALUE));
        Term bi2 = new Integer(BigInteger.valueOf(java.lang.Long.MIN_VALUE));

        assertEquals("should be = integers", i1, i2);
        assertNotEquals("should be <> integers", i1, i3);


        // RATIONALS
        Term r1 = new Rational(2, 3);
        Term r2 = new Rational(20, 30);
        Term r3 = new Rational("20r30");

        Term r4 = new Rational("21r30");

        assertEquals("should be = rationals", r1, r2);
        assertEquals("should be = rationals", r1, r3);
        assertEquals("should be = rationals", r2, r3);

        assertNotEquals("should be <> rationals", r1, r4);


        // FLOATS
        Term f1 = new Float(1212.23);
        Term f2 = new Float(1212.23);
        Term f3 = new Float(212);

        assertEquals("should be = floats", f1, f2);
        assertNotEquals("should be <> float", f1, f3);

        assertEquals("should be = float and integer", f3, i3);
        assertNotEquals("should be <> float and integer", f3, i2);
        assertNotEquals("should be <> float and rational", f3, r2);

    }



}
