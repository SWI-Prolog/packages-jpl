package org.jpl7.junit;

import org.jpl7.*;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class Test_Exceptions extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.junit.Test_Exceptions");

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
