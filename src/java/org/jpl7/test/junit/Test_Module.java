package org.jpl7.test.junit;

import org.jpl7.PrologException;
import org.jpl7.Query;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import static org.junit.Assert.*;


public class Test_Module extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.test.junit.Test_Module");

        // should work from static class but gives error
//        org.junit.runner.JUnitCore.main( GetSolution.class.getName()); // full name with package
    }

    /**
     * This is done at the class loading, before any test is run
     */
    @BeforeClass
    public static void setUp() {
        setUpClass();

        useJPLmodule();     // consult the jpl.pl module
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
    public void testGoalWithModulePrefix1() {
        String goal = "jpl:jpl_modifier_bit(volatile,I)";
        assertTrue(goal + " binds I to an integer", Query.oneSolution(goal).get("I").isInteger());
    }

    @Test
    public void testGoalWithModulePrefix2() {
        String goal = "user:length([],0)";
        assertTrue(goal + " succeeds", Query.hasSolution(goal));
    }

    @Test
    public void testGoalWithModulePrefix3() {
        String goal = "3:length([],0)";
        try {
            Query.hasSolution(goal); // should throw exception
            fail(goal + " (numeric module prefix) didn't throw exception"); // shouldn't
            // get
            // to
            // here
        } catch (PrologException e) { // expected exception class
            if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("type_error", 2)
                    && e.term().arg(1).arg(1).hasFunctor("atom", 0)) {
                // OK: an appropriate exception was thrown
            } else {
                fail(goal + " (numeric module prefix) threw incorrect PrologException: " + e);
            }
        } catch (Exception e) {
            fail(goal + " (numeric module prefix) threw wrong class of exception: " + e);
        }
    }

    @Test
    public void testGoalWithModulePrefix4() {
        String goal = "_:length([],0)";
        try {
            Query.hasSolution(goal); // should throw exception
            fail(goal + " (unbound module prefix) wrongly succeeded"); // shouldn't
            // get
            // to
            // here
        } catch (PrologException e) { // expected exception class
            if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("instantiation_error", 0)) {
                // OK: an appropriate exception was thrown
            } else {
                fail(goal + " (unbound module prefix) threw wrong PrologException: " + e);
            }
        } catch (Exception e) {
            fail(goal + " (unbound module prefix) threw wrong exception class: " + e);
        }
    }

    @Test
    public void testGoalWithModulePrefix5() {
        String goal = "f(x):length([],0)";
        try {
            Query.hasSolution(goal); // should throw exception
            fail(goal + " (compound module prefix) wrongly succeeded"); // shouldn't
            // get
            // to
            // here
        } catch (PrologException e) { // correct exception class
            if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("type_error", 2)
                    && e.term().arg(1).arg(1).hasFunctor("atom", 0)) {
                // OK: an appropriate exception was thrown
            } else {
                fail(goal + " (compound module prefix) threw wrong PrologException: " + e);
            }
        } catch (Exception e) {
            fail(goal + " (compound module prefix) threw wrong exception class: " + e);
        }
    }

    @Test
    public void testGoalWithModulePrefix6() {
        String goal = "no_such_module:no_such_predicate(0)";
        try {
            Query.hasSolution(goal); // should throw exception
            fail(goal + " (nonexistent module prefix) wrongly succeeded"); // shouldn't
            // get
            // to
            // here
        } catch (PrologException e) { // expected exception class
            if (e.term().hasFunctor("error", 2) && e.term().arg(1).hasFunctor("existence_error", 2)
                    && e.term().arg(1).arg(1).hasFunctor("procedure", 0)) {
                // OK: an appropriate exception was thrown
            } else {
                fail(goal + " (nonexistent module prefix) threw wrong PrologException: " + e);
            }
        } catch (Exception e) {
            fail(goal + " (nonexistent module prefix) threw wrong exception class: " + e);
        }
    }



    @Test
    public void testModulePrefix1() {
        assertTrue(Query.hasSolution("call(user:true)"));
    }

}
