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
import java.util.Set;

import static org.junit.Assert.*;

/**
 * If we override .equals() we must also override .hascode()
 *
 * https://programming.guide/java/overriding-hashcode-and-equals.html
 *
 */
public class Test_Equals extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.test.junit.Test_Equals");

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


    ////////////////////////////////////////////////////
    //// ATOM
    ////////////////////////////////////////////////////

    @Test
    public void test_atom_equal1() {
        Atom a = new Atom("fred");
        Atom b = new Atom("fred");
        assertEquals("atoms should be equal", a, b);
    }


    @Test
    public void test_atom_equal2() {
        Atom a = new Atom("age(john)");
        Atom b = new Atom("age(john)");
        assertEquals("atoms should be equal", a, b);
    }

    @Test
    public void test_atom_equal3() {
        Atom a = new Atom("fred");
        Atom b = new Atom("alice");
        assertNotEquals("atoms should not be equal", a, b);
    }

    @Test
    public void test_atom_equal4() {
        Atom a = new Atom("age(john)");
        Atom b = new Atom("age(mary)");
        assertNotEquals("atoms should not be equal", a, b);
    }

    @Test
    public void test_atom_equals4() {
        Atom a = new Atom("here", "text");
        Atom b = new Atom("here",  "string");

        Set<Atom> atomBoxes = new HashSet<>();
        atomBoxes.add(a);

        assertNotEquals("should not be equal - different types", a, b);
    }


    @Test
    public void test_atom_hash1() {
        Atom a = new Atom("age(john)");
        Atom b = new Atom("age(john)");

        Set<Atom> atomBoxes = new HashSet<>();
        atomBoxes.add(a);

        assertTrue("b should be contained in set (= hash)", atomBoxes.contains(b));
    }

    @Test
    public void test_atom_hash2() {
        Atom a = new Atom("string", "text");
        Atom b = new Atom("text",  "string");

        Set<Atom> atomBoxes = new HashSet<>();
        atomBoxes.add(a);

        assertFalse("b should NOT be contained in set (<> hash)", atomBoxes.contains(b));
    }

    @Test
    public void test_atom_hash3() {
        Atom a = new Atom("here", "text");
        Atom b = new Atom("here",  "string");

        Set<Atom> atomBoxes = new HashSet<>();
        atomBoxes.add(a);

        assertFalse("b should NOT be contained in set (<> hash)", atomBoxes.contains(b));
    }

    ////////////////////////////////////////////////////
    //// COMPOUNDS
    ////////////////////////////////////////////////////

    @Test
    public void test_compound_equals1() {

        Term t1 = Term.textToTerm("mother(rosana,lucia)");
        Term t2 = Term.textToTerm("mother(rosana, lucia)");
        Term t3 = Term.textToTerm("mother(rosana, marcos)");

        assertNotSame("objects t1 and t2  are not the same", t1, t2);
        assertEquals("objects t1 and t2 are equal", t1, t2);


        assertNotEquals("objects t1 and t3 are not equal", t1, t3);
    }

    @Test
    public void test_compound_hash1() {
        Term t1 = Term.textToTerm("mother(rosana,lucia)");
        Term t2 = Term.textToTerm("mother(rosana, lucia)");
        Term t3 = Term.textToTerm("mother(rosana, marcos)");

        Set<Term> compBoxes = new HashSet<>();
        compBoxes.add(t1);

        assertTrue("t2 should be contained in set (<> hash)", compBoxes.contains(t2));

        assertFalse("t3 should NOT be contained in set (<> hash)", compBoxes.contains(t3));

    }



    ////////////////////////////////////////////////////
    //// VARIABLE
    ////////////////////////////////////////////////////



    @Test
    public void test_variable_equal1() {
        Variable v1 = new Variable("X");
        Variable v2 = new Variable("X");

        Term s1 = new Query("? = 5", v1).oneSolution().get("X");
        Term s2 = new Query("? = 15", v2).oneSolution().get("X");

        assertNotEquals("Bindings should be different (despite same var name)",
                s1.toString(), s2.toString());

        assertNotEquals("variables are indeed not equal despite = names", v1, v2);
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

        assertFalse("v2 should not be contained in set (<> hash)", boxes.contains(v2));
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
