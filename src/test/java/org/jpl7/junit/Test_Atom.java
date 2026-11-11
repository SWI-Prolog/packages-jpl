package org.jpl7.junit;

import org.jpl7.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

public class Test_Atom extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.jpl7.JPLTestSuiteRunner.run(Test_Atom.class); // full name with package
    }

    @Test
    public void testAtom1() {
        assertTrue(true, "new Atom(\"3 3\")" + (new Atom("3 3")).toString());
    }

    @Test
    public void testAtomName1() {
        String name = "fred";
        Atom a = new Atom(name);
        assertEquals(a.name(), name, "an Atom's name is that with which it was created");
    }

    @Test
    public void testAtomName2() {
        String name = "ha ha";
        Atom a = new Atom(name);
        assertEquals(a.name(), name, "an Atom's name is that with which it was created");
    }

    @Test
    public void testAtomName3() {
        String name = "3";
        Atom a = new Atom(name);
        assertEquals(a.name(), name, "an Atom's name is that with which it was created");
    }

    @Test
    public void testAtomToString1() {
        String name = "fred";
        String toString = "fred";
        Atom a = new Atom(name);
        assertEquals(a.toString(), toString, "an Atom's .toString() value is quoted iff appropriate");
    }

    @Test
    public void testAtomToString2() {
        String name = "ha ha";
        String toString = "'ha ha'";
        Atom a = new Atom(name);
        assertEquals(a.toString(), toString, "an Atom's .toString() value is quoted iff appropriate");
    }

    @Test
    public void testAtomToString3() {
        String name = "3";
        String toString = "'3'";
        Atom a = new Atom(name);
        assertEquals(a.toString(), toString, "an Atom's .toString() value is quoted iff appropriate");
    }

    @Test
    public void testAtomArity() {
        Atom a = new Atom("willy");
        assertEquals(a.arity(), 0, "an Atom has arity zero");
    }

    @Test
    public void testAtomEquality1() {
        String name = "fred";
        Atom a1 = new Atom(name);
        Atom a2 = new Atom(name);
        assertEquals(a1, a2, "two Atoms created with the same name are equal");
    }

    @Test
    public void testAtomIdentity() { // how could this fail?!
        String name = "fred";
        Atom a1 = new Atom(name);
        Atom a2 = new Atom(name);
        assertNotSame(a1, a2, "two Atoms created with the same name are not identical");
    }

    @Test
    public void testAtomHasFunctorNameZero() {
        String name = "sam";
        Atom a = new Atom(name);
        assertTrue(a.hasFunctor(name, 0), "a text atom has a functor whose name is the name of the atom, and whose arity is zero");
    }

    @Test
    public void testAtomHasFunctorWrongName() {
        assertFalse(new Atom("wally").hasFunctor("poo", 0), "an Atom does not have a functor whose name is other than that with which the Atom was created");
    }

    @Test
    public void testAtomHasFunctorWrongArity() {
        String name = "ted";
        assertFalse(new Atom(name).hasFunctor(name, 1), "an Atom does not have a functor whose arity is other than zero");
    }

    @Test
    public void testAtomEquality2() {
        Atom a = new Atom("a");
        //noinspection EqualsWithItself
        assertEquals(a, a, "two references to an Atom are equal by .equals()");
    }

    @Test
    public void testAtomEquality3() {
        assertEquals(new Atom("a"), new Atom("a"), "two distinct, same-named Atoms are equal by .equals()");
    }
}
