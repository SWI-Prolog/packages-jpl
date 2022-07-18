package org.jpl7.junit;

import java.util.HashSet;
import java.util.Set;
import org.jpl7.*;
import org.jpl7.Float; // prefer to java.lang.Float
import org.jpl7.Integer; // prefer to java.lang.Integer
import static org.junit.Assert.*;
import org.junit.Test;

/**
 * If we override .equals() we must also override .hascode()
 *
 * https://programming.guide/java/overriding-hashcode-and-equals.html
 *
 */
public class Test_Equals extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main(Test_Equals.class.getName()); // full name with package
    }

    @Test
    public void test_integer_equal() {
        Term i1 = new Integer(1212);
        Term i2 = new Integer(1212);
        Term i3 = new Integer(212);
        assertEquals("should be = integers", i1, i2);
        assertNotEquals("should be <> integers", i1, i3);
    };

    // TODO: big integers 

    @Test
    public void test_rational_equal() {
        Term r1 = new Rational(2, 3);
        Term r2 = new Rational(20, 30);
        Term r3 = new Rational("20r30");
        Term r4 = new Rational("21r30");
        assertEquals("should be = rationals", r1, r2);
        assertEquals("should be = rationals", r1, r3);
        assertEquals("should be = rationals", r2, r3);
        assertNotEquals("should be <> rationals", r1, r4);
    };

    @Test
    public void test_float_equal() {
        Term f1 = new Float(1212.23);
        Term f2 = new Float(1212.23);
        Term f3 = new Float(212);
        assertEquals("should be = floats", f1, f2);
        assertNotEquals("should be <> float", f1, f3);
    };

    @Test
    public void test_mixed_equal() {
        Term f3 = new Float(212);
        Term i2 = new Integer(1212);
        Term i3 = new Integer(212);
        Term r2 = new Rational(20, 30);
        assertEquals("should be = float and integer", f3, i3);
        assertNotEquals("should be <> float and integer", f3, i2);
        assertNotEquals("should be <> float and rational", f3, r2);
    }

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
    public void test_atom_equal5() {
        Atom a = new Atom("here", "text");
        Atom b = new Atom("here", "string");
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
        Atom b = new Atom("text", "string");
        Set<Atom> atomBoxes = new HashSet<>();
        atomBoxes.add(a);
        assertFalse("b should NOT be contained in set (<> hash)", atomBoxes.contains(b));
    }

    @Test
    public void test_atom_hash3() {
        Atom a = new Atom("here", "text");
        Atom b = new Atom("here", "string");
        Set<Atom> atomBoxes = new HashSet<>();
        atomBoxes.add(a);
        assertFalse("b should NOT be contained in set (<> hash)", atomBoxes.contains(b));
    }

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
}
