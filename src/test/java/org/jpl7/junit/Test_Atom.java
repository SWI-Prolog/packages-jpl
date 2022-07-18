package org.jpl7.junit;

import org.jpl7.*;
import static org.junit.Assert.*;
import org.junit.Test;

public class Test_Atom extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main(Test_Atom.class.getName()); // full name with package
    }

    @Test
    public void testAtom1() {
        assertTrue("new Atom(\"3 3\")" + (new Atom("3 3")).toString(), true);
    }

    @Test
    public void testAtomName1() {
        String name = "fred";
        Atom a = new Atom(name);
        assertEquals("an Atom's name is that with which it was created", a.name(), name);
    }

    @Test
    public void testAtomName2() {
        String name = "ha ha";
        Atom a = new Atom(name);
        assertEquals("an Atom's name is that with which it was created", a.name(), name);
    }

    @Test
    public void testAtomName3() {
        String name = "3";
        Atom a = new Atom(name);
        assertEquals("an Atom's name is that with which it was created", a.name(), name);
    }

    @Test
    public void testAtomToString1() {
        String name = "fred";
        String toString = "fred";
        Atom a = new Atom(name);
        assertEquals("an Atom's .toString() value is quoted iff appropriate", a.toString(), toString);
    }

    @Test
    public void testAtomToString2() {
        String name = "ha ha";
        String toString = "'ha ha'";
        Atom a = new Atom(name);
        assertEquals("an Atom's .toString() value is quoted iff appropriate", a.toString(), toString);
    }

    @Test
    public void testAtomToString3() {
        String name = "3";
        String toString = "'3'";
        Atom a = new Atom(name);
        assertEquals("an Atom's .toString() value is quoted iff appropriate", a.toString(), toString);
    }

    @Test
    public void testAtomArity() {
        Atom a = new Atom("willy");
        assertEquals("an Atom has arity zero", a.arity(), 0);
    }

    @Test
    public void testAtomEquality1() {
        String name = "fred";
        Atom a1 = new Atom(name);
        Atom a2 = new Atom(name);
        assertEquals("two Atoms created with the same name are equal", a1, a2);
    }

    @Test
    public void testAtomIdentity() { // how could this fail?!
        String name = "fred";
        Atom a1 = new Atom(name);
        Atom a2 = new Atom(name);
        assertNotSame("two Atoms created with the same name are not identical", a1, a2);
    }

    @Test
    public void testAtomHasFunctorNameZero() {
        String name = "sam";
        Atom a = new Atom(name);
        assertTrue("a text atom has a functor whose name is the name of the atom, and whose arity is zero",
                a.hasFunctor(name, 0));
    }

    @Test
    public void testAtomHasFunctorWrongName() {
        assertFalse("an Atom does not have a functor whose name is other than that with which the Atom was created",
                new Atom("wally").hasFunctor("poo", 0));
    }

    @Test
    public void testAtomHasFunctorWrongArity() {
        String name = "ted";
        assertFalse("an Atom does not have a functor whose arity is other than zero",
                new Atom(name).hasFunctor(name, 1));
    }

    @Test
    public void testAtomEquality2() {
        Atom a = new Atom("a");
        //noinspection EqualsWithItself
        assertEquals("two references to an Atom are equal by .equals()", a, a);
    }

    @Test
    public void testAtomEquality3() {
        assertEquals("two distinct, same-named Atoms are equal by .equals()", new Atom("a"), new Atom("a"));
    }
}
