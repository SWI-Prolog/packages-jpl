package org.jpl7.test.junit;

import org.jpl7.*;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import static org.junit.Assert.*;


public class TypesTest extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.test.junit.TypesTest");

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
    public void testIsJNull1() {
        Term atNull = new Compound("@", new Term[]{new Atom("null")});
        Query q = new Query("=", new Term[]{new Variable("X"), atNull});
        assertTrue(q.oneSolution().get("X").isJNull());
    }

    @Test
    public void testIsJNull2() {
        Term t = Query.oneSolution("X = @(3)").get("X");
        assertFalse("@(3) . isJNull() fails", t.isJNull());
    }

    @Test
    public void testIsJNull3() {
        Term t = Query.oneSolution("X = _").get("X");
        assertFalse("_ . isJNull() fails", t.isJNull());
    }

    @Test
    public void testIsJNull4() {
        Term t = Query.oneSolution("X = @(true)").get("X");
        assertFalse("@(true) . isJNull() fails", t.isJNull());
    }

    @Test
    public void testIsJNull5() {
        Term t = Query.oneSolution("X = @(false)").get("X");
        assertFalse("@(false) . isJNull() fails", t.isJNull());
    }

    @Test
    public void testIsJTrue1() {
        Term t = Query.oneSolution("X = @(true)").get("X");
        assertTrue("@(true) . isJTrue() succeeds", t.isJTrue());
    }

    @Test
    public void testIsJTrue2() {
        Term t = Query.oneSolution("X = @(3)").get("X");
        assertFalse("@(3) . isJTrue() fails", t.isJTrue());
    }

    @Test
    public void testIsJTrue3() {
        Term t = Query.oneSolution("X = _").get("X");
        assertFalse("_ . isJTrue() fails", t.isJTrue());
    }

    @Test
    public void testIsJTrue4() {
        Term t = Query.oneSolution("X = @(false)").get("X");
        assertFalse("@(false) . isJTrue() fails", t.isJTrue());
    }

    @Test
    public void testIsJVoid1() {
        Term t = Query.oneSolution("X = @(void)").get("X");
        assertTrue("@(void) . isJVoid() succeeds", t.isJVoid());
    }

    @Test
    public void testIsJVoid2() {
        Term t = Query.oneSolution("X = @(3)").get("X");
        assertFalse("@(3) . isJVoid() fails", t.isJVoid());
    }

    @Test
    public void testIsJVoid3() {
        Term t = Query.oneSolution("X = _").get("X");
        assertFalse("_ . isJVoid() fails", t.isJVoid());
    }

    @Test
    public void testTypeName1() {
        assertEquals("Y = foo binds Y to an Atom", Query.oneSolution("Y = foo").get("Y").typeName(), "Atom");
    }

    @Test
    public void testTypeName2() {
        assertEquals("Y = 3.14159 binds Y to a Float", Query.oneSolution("Y = 3.14159").get("Y").typeName(), "Float");
    }

    @Test
    public void testTypeName4() {
        assertEquals("Y = 6 binds Y to an Integer", Query.oneSolution("Y = 6").get("Y").typeName(), "Integer");
    }

    @Test
    public void testTypeName5() {
        assertEquals("Y = _ binds Y to a Variable", Query.oneSolution("Y = _").get("Y").typeName(), "Variable");
    }

    @Test
    public void testTypeName3() {
        assertEquals("Y = f(x) binds Y to a Compound", Query.oneSolution("Y = f(x)").get("Y").typeName(), "Compound");
    }


}
