package org.jpl7.junit;

import org.jpl7.*;

import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.util.Map;

import static org.junit.Assert.*;

public class Test_Rational extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.junit.Test_Rational");

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
    public void rational_simple() {
        Query q = new Query("X = 12r5");
        Map<String, Term> s = q.nextSolution();

        assertEquals( "12r5", s.get("X").toString());
        assertEquals("Rational", s.get("X").typeName());
    }

    @Test
    public void rational_reduced() {
        Query q = new Query("X = 2r4");
        Map<String, Term> s = q.nextSolution();

        assertEquals("1r2", s.get("X").toString());
    }


    @Test
    public void rational_is_simple() {
        final int[] expectedSolutions = {1, 2, 3, 4, 5};

//        Rational rat = new Rational("X = 1r4");
//        System.out.println(rat.toString());

        Query q = new Query("X is 2r4 + 1r3");
        Map<String, Term> s = q.nextSolution();
        assertEquals("5r6", s.get("X").toString());
    }

    @Test
    public void rational_is_simple2() {
        Query q = new Query("X is 1r2 + 1r4");
        Map<String, Term> s = q.nextSolution();
        assertEquals("3r4", s.get("X").toString());
    }

    @Test
    public void rational_is_big() {
        Query q = new Query("X is 123r13 + 123r8");
        Map<String, Term> s = q.nextSolution();
        assertEquals("2583r104", s.get("X").toString());
    }


    @Test
    public void rational_is_reduced_to_int() {
        Query q = new Query("X is 1r2 + 1r2");
        Map<String, Term> s = q.nextSolution();
        assertEquals(1, s.get("X").intValue());
        assertEquals("Integer", s.get("X").typeName());

    }


    @Test
    public void rational_from_String_simple() {
        Rational rat = new Rational("1r4");

        Query q = new Query("X is ?", rat);
        Map<String, Term> s = q.nextSolution();
        assertEquals("1r4", s.get("X").toString());
    }


    @Test
    public void rational_from_String_reduced() {
        final int[] expectedSolutions = {1, 2, 3, 4, 5};

        Rational rat = new Rational("2r8");

        Query q = new Query("X is ?", rat);
        Map<String, Term> s = q.nextSolution();
        assertEquals("1r4", s.get("X").toString());
    }

    @Test
    public void rational_from_String_wrong_format1() {
        final String[] wrongRationals = {"121", "22r", "r33", "ss", "-12r", "r-21"};
        final String expectedError = "incorrect format for rational number";

        for (String r : wrongRationals) {
            try {
                Rational rat = new Rational(r);
            } catch (JPLException e) {
                machExceptionError(e, expectedError);
            }
        }
    }

    @Test
    public void rational_from_String_wrong_format2() {
        final String[] wrongRationals = {"23r0", "-22r0"};
        final String expectedError = "denominator of rational cannot be 0";

        for (String r : wrongRationals) {
            try {
                Rational rat = new Rational(r);
            } catch (JPLException e) {
                machExceptionError(e, expectedError);
            }
        }
    }


    @Test
    public void rational_from_Prolog_multiply() {
        final int[] expectedSolutions = {1, 2, 3, 4, 5};

        Rational rat1 = new Rational("2r8");
        Rational rat2 = new Rational("3r2");

        Query q = new Query("X is ? * ?", new Term[] {rat1, rat2});
        Map<String, Term> s = q.nextSolution();
        assertEquals("3r8", s.get("X").toString());
    }




}
