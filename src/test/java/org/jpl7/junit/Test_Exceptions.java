package org.jpl7.junit;

import org.jpl7.*;
import static org.junit.Assert.*;
import org.junit.Test;

public class Test_Exceptions extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main(Test_Exceptions.class.getName()); // full name with package
    }

    @Test
    public void test0() {
        final String expectedError = "instantiation_error";
        try {
            Query.hasSolution("X is Y");
            fail("Should have given an exception!");
        } catch (PrologException e) {
            machExceptionError(e, expectedError);
        }
    }

    @Test
    public void test1() {
        final String expectedError = "existence_error";
        try {
            Query.hasSolution("mod:notexists(2)");
            fail("Should have given an exception!");
        } catch (PrologException e) {
            machExceptionError(e, expectedError);
        }
    }

    @Test
    public void test3() {
        final String expectedError = "existence_error";
        try {
            Query.hasSolution("mod:notexists(2)");
            fail("Should have given an exception!");
        } catch (PrologException e) {
            machExceptionError(e, expectedError);
        }
    }

    @Test
    public void test4() {
        final String[] wrongRationals = {"121", "22r", "r33", "ss", "-12r", "r-21"};
        final String expectedError = "incorrect format for rational number";
        for (String s : wrongRationals) {
	        try {
	            new Rational(s);
	            fail("new Rational(" + s + ") should have thrown \"incorrect format for rational number\"");
	        } catch (JPLException e) {
	            machExceptionError(e, expectedError);
	        }
        }
    }
}
