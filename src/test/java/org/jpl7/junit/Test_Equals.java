package org.jpl7.junit;

import java.util.HashSet;
import java.util.Set;
import org.jpl7.*;
import org.jpl7.Float; // prefer to java.lang.Float
import org.jpl7.Integer; // prefer to java.lang.Integer
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

/**
 * If we override .equals() we must also override .hascode()
 *
 * https://programming.guide/java/overriding-hashcode-and-equals.html
 *
 */
public class Test_Equals extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.jpl7.JPLTestSuiteRunner.run(Test_Equals.class); // full name with package
    }

    @Test
    public void test_integer_equal() {
        Term i1 = new Integer(1212);
        Term i2 = new Integer(1212);
        Term i3 = new Integer(212);
        assertEquals(i1, i2, "should be = integers");
        assertNotEquals(i1, i3, "should be <> integers");
    };

    // TODO: big integers 

    @Test
    public void test_rational_equal() {
        Term r1 = new Rational(2, 3);
        Term r2 = new Rational(20, 30);
        Term r3 = new Rational("20r30");
        Term r4 = new Rational("21r30");
        assertEquals(r1, r2, "should be = rationals");
        assertEquals(r1, r3, "should be = rationals");
        assertEquals(r2, r3, "should be = rationals");
        assertNotEquals(r1, r4, "should be <> rationals");
    };

    @Test
    public void test_float_equal() {
        Term f1 = new Float(1212.23);
        Term f2 = new Float(1212.23);
        Term f3 = new Float(212);
        assertEquals(f1, f2, "should be = floats");
        assertNotEquals(f1, f3, "should be <> float");
    };

    @Test
    public void test_mixed_equal() {
        Term f3 = new Float(212);
        Term i2 = new Integer(1212);
        Term i3 = new Integer(212);
        Term r2 = new Rational(20, 30);
        assertEquals(f3, i3, "should be = float and integer");
        assertNotEquals(f3, i2, "should be <> float and integer");
        assertNotEquals(f3, r2, "should be <> float and rational");
    }

    @Test
    public void test_atom_equal1() {
        Atom a = new Atom("fred");
        Atom b = new Atom("fred");
        assertEquals(a, b, "atoms should be equal");
    }

    @Test
    public void test_atom_equal2() {
        Atom a = new Atom("age(john)");
        Atom b = new Atom("age(john)");
        assertEquals(a, b, "atoms should be equal");
    }

    @Test
    public void test_atom_equal3() {
        Atom a = new Atom("fred");
        Atom b = new Atom("alice");
        assertNotEquals(a, b, "atoms should not be equal");
    }

    @Test
    public void test_atom_equal4() {
        Atom a = new Atom("age(john)");
        Atom b = new Atom("age(mary)");
        assertNotEquals(a, b, "atoms should not be equal");
    }

    @Test
    public void test_atom_equal5() {
        Atom a = new Atom("here", "text");
        Atom b = new Atom("here", "string");
        assertNotEquals(a, b, "should not be equal - different types");
    }

    @Test
    public void test_atom_hash1() {
        Atom a = new Atom("age(john)");
        Atom b = new Atom("age(john)");
        Set<Atom> atomBoxes = new HashSet<>();
        atomBoxes.add(a);
        assertTrue(atomBoxes.contains(b), "b should be contained in set (= hash)");
    }

    @Test
    public void test_atom_hash2() {
        Atom a = new Atom("string", "text");
        Atom b = new Atom("text", "string");
        Set<Atom> atomBoxes = new HashSet<>();
        atomBoxes.add(a);
        assertFalse(atomBoxes.contains(b), "b should NOT be contained in set (<> hash)");
    }

    @Test
    public void test_atom_hash3() {
        Atom a = new Atom("here", "text");
        Atom b = new Atom("here", "string");
        Set<Atom> atomBoxes = new HashSet<>();
        atomBoxes.add(a);
        assertFalse(atomBoxes.contains(b), "b should NOT be contained in set (<> hash)");
    }

    @Test
    public void test_compound_equals1() {
        Term t1 = Term.textToTerm("mother(rosana,lucia)");
        Term t2 = Term.textToTerm("mother(rosana, lucia)");
        Term t3 = Term.textToTerm("mother(rosana, marcos)");
        assertNotSame(t1, t2, "objects t1 and t2  are not the same");
        assertEquals(t1, t2, "objects t1 and t2 are equal");
        assertNotEquals(t1, t3, "objects t1 and t3 are not equal");
    }

    @Test
    public void test_compound_hash1() {
        Term t1 = Term.textToTerm("mother(rosana,lucia)");
        Term t2 = Term.textToTerm("mother(rosana, lucia)");
        Term t3 = Term.textToTerm("mother(rosana, marcos)");
        Set<Term> compBoxes = new HashSet<>();
        compBoxes.add(t1);
        assertTrue(compBoxes.contains(t2), "t2 should be contained in set (<> hash)");
        assertFalse(compBoxes.contains(t3), "t3 should NOT be contained in set (<> hash)");
    }
}
