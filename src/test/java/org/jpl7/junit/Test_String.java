package org.jpl7.junit;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import static org.junit.Assert.assertEquals;


public class Test_String extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.junit.Test_String");

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
    public void testStringXput1() {
        Term a = Query.oneSolution("string_concat(foo,bar,S)").get("S");
        assertEquals("foobar", a.name());
        assertEquals("string", a.atomType());
    }

    @Test
    public void testStringXput2() {
        String s1 = "\u0000\u007F\u0080\u00FF";
        String s2 = "\u0100\u7FFF\u8000\uFFFF";
        String s = s1 + s2; // concatenate in Java
        Term a = Query.oneSolution("string_concat(?,?,S)", new Term[]{new Atom(s1), new Atom(s2)}).get("S"); // concatenate
        // in
        // Prolog
        assertEquals(s, a.name());
        assertEquals("string", a.atomType());
    }



}
