package org.jpl7.junit;

import org.jpl7.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

public class Test_Types extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.jpl7.JPLTestSuiteRunner.run(Test_Types.class); // full name with package
    }

    @Test
    public void testIsJNull1() {
        Term atNull = new Compound("@", new Term[]{new Atom("null")});
        Query q = new Query("=", new Term[]{new Variable("X"), atNull});
        assertTrue(q.oneSolution().get("X").isJNull());
    }

    @Test
    public void testIsJNull2() {
        Term t = Query.oneSolution("X = @(3)").get("X");
        assertFalse(t.isJNull(), "@(3) . isJNull() fails");
    }

    @Test
    public void testIsJNull3() {
        Term t = Query.oneSolution("X = _").get("X");
        assertFalse(t.isJNull(), "_ . isJNull() fails");
    }

    @Test
    public void testIsJNull4() {
        Term t = Query.oneSolution("X = @(true)").get("X");
        assertFalse(t.isJNull(), "@(true) . isJNull() fails");
    }

    @Test
    public void testIsJNull5() {
        Term t = Query.oneSolution("X = @(false)").get("X");
        assertFalse(t.isJNull(), "@(false) . isJNull() fails");
    }

    @Test
    public void testIsJTrue1() {
        Term t = Query.oneSolution("X = @(true)").get("X");
        assertTrue(t.isJTrue(), "@(true) . isJTrue() succeeds");
    }

    @Test
    public void testIsJTrue2() {
        Term t = Query.oneSolution("X = @(3)").get("X");
        assertFalse(t.isJTrue(), "@(3) . isJTrue() fails");
    }

    @Test
    public void testIsJTrue3() {
        Term t = Query.oneSolution("X = _").get("X");
        assertFalse(t.isJTrue(), "_ . isJTrue() fails");
    }

    @Test
    public void testIsJTrue4() {
        Term t = Query.oneSolution("X = @(false)").get("X");
        assertFalse(t.isJTrue(), "@(false) . isJTrue() fails");
    }

    @Test
    public void testIsJVoid1() {
        Term t = Query.oneSolution("X = @(void)").get("X");
        assertTrue(t.isJVoid(), "@(void) . isJVoid() succeeds");
    }

    @Test
    public void testIsJVoid2() {
        Term t = Query.oneSolution("X = @(3)").get("X");
        assertFalse(t.isJVoid(), "@(3) . isJVoid() fails");
    }

    @Test
    public void testIsJVoid3() {
        Term t = Query.oneSolution("X = _").get("X");
        assertFalse(t.isJVoid(), "_ . isJVoid() fails");
    }

    @Test
    public void testTypeName1() {
        assertEquals(Query.oneSolution("Y = foo").get("Y").typeName(), "Atom", "Y = foo binds Y to an Atom");
    }

    @Test
    public void testTypeName2() {
        assertEquals(Query.oneSolution("Y = 3.14159").get("Y").typeName(), "Float", "Y = 3.14159 binds Y to a Float");
    }

    @Test
    public void testTypeName4() {
        assertEquals(Query.oneSolution("Y = 6").get("Y").typeName(), "Integer", "Y = 6 binds Y to an Integer");
    }

    @Test
    public void testTypeName5() {
        assertEquals(Query.oneSolution("Y = _").get("Y").typeName(), "Variable", "Y = _ binds Y to a Variable");
    }

    @Test
    public void testTypeName3() {
        assertEquals(Query.oneSolution("Y = f(x)").get("Y").typeName(), "Compound", "Y = f(x) binds Y to a Compound");
    }
}
