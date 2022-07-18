package org.jpl7.junit;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class Test_String extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main(Test_String.class.getName()); // full name with package
    }

    @Test
    public void testStringXput1() {
        Term a = Query.oneSolution("string_concat(foo,bar,S)").get("S");
        assertEquals("foobar", a.name());
        assertEquals("string", a.atomType());
    }

    @Test
    public void testStringXput2() {
        String s1 = "\u0000\u007F\u0080\u00FF";
        String s2 = "\u0100\u7FFF\u8000\uFFFF";
        String s = s1 + s2; // catenate in Java
        Term a1 = new Atom(s1);
        Term a2 = new Atom(s2);
        Term a = Query.oneSolution("string_concat(?,?,S)", new Term[]{a1, a2}).get("S"); // catenate in Prolog
        assertEquals(s, a.name());
        assertEquals("string", a.atomType());
    }
}
