package org.jpl7.junit;

import org.jpl7.*;
import org.junit.Test;

import static org.junit.Assert.*;

public class Test_Unicode extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main(Test_Unicode.class.getName()); // full name with package
    }

    @Test
    public void testUnicode0() {
        assertTrue(Query.hasSolution("atom_codes(?,[32])", new Term[]{new Atom(" ")}));
    }

    @Test
    public void testUnicode0a() {
        assertTrue(Query.hasSolution("atom_codes(?,[32])", new Term[]{new Atom("\u0020")}));
    }

    @Test
    public void testUnicode0b() {
        assertTrue(Query.hasSolution("atom_codes(?,[0])", new Term[]{new Atom("\u0000")}));
    }

    @Test
    public void testUnicode0c() {
        assertTrue(Query.hasSolution("atom_codes(?,[1])", new Term[]{new Atom("\u0001")}));
    }

    @Test
    public void testUnicode0d() {
        assertTrue(Query.hasSolution("atom_codes(?,[127])", new Term[]{new Atom("\u007F")}));
    }

    @Test
    public void testUnicode0e() {
        assertTrue(Query.hasSolution("atom_codes(?,[128])", new Term[]{new Atom("\u0080")}));
    }

    @Test
    public void testUnicode0f() {
        assertTrue(Query.hasSolution("atom_codes(?,[255])", new Term[]{new Atom("\u00FF")}));
    }

    @Test
    public void testUnicode0g() {
        assertTrue(Query.hasSolution("atom_codes(?,[256])", new Term[]{new Atom("\u0100")}));
    }

    @Test
    public void testUnicode1() {
        assertTrue(Query.hasSolution("atom_codes(?,[0,127,128,255])",
                new Term[]{new Atom("\u0000\u007F\u0080\u00FF")}));
    }

    @Test
    public void testUnicode2() {
        assertTrue(Query.hasSolution("atom_codes(?,[256,32767,32768,65535])",
                new Term[]{new Atom("\u0100\u7FFF\u8000\uFFFF")}));
    }
}
