package org.jpl7.test.junit;

import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.Term;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

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
        assertTrue("atoms should be equal", a.equals(b));
    }


    @Test
    public void test_atom_equal2() {
        Atom a = new Atom("age(john)");
        Atom b = new Atom("age(john)");
        assertTrue("atoms should be equal", a.equals(b));
    }

    @Test
    public void test_atom_equal3() {
        Atom a = new Atom("fred");
        Atom b = new Atom("alice");
        assertFalse("atoms should not be equal", a.equals(b));
    }

    @Test
    public void test_atom_equal4() {
        Atom a = new Atom("age(john)");
        Atom b = new Atom("age(mary)");
        assertFalse("atoms should not be equal", a.equals(b));
    }

    @Test
    public void test_atom_equals4() {
        Atom a = new Atom("here", "text");
        Atom b = new Atom("here",  "string");

        Set<Atom> atomBoxes = new HashSet<>();
        atomBoxes.add(a);

        assertFalse("should not be equal - different types", a.equals(b));
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

        assertFalse("objects t1 and t2  are not the same", t1 == t2);
        assertTrue("objects t1 and t2 are equal", t1.equals(t2));


        assertFalse("objects t1 and t3 are not equal", t1.equals(t3));
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
