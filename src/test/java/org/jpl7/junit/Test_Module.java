package org.jpl7.junit;

import org.jpl7.*;
import static org.junit.Assert.*;
import org.junit.Test;

public class Test_Module extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main(Test_Module.class.getName()); // full name with package
    }

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
        String goal = "3:length([],0)"; // invalid module prefix (numeric)
        try {
            Query.hasSolution(goal); // should throw exception
            fail(goal + " (numeric module prefix) didn't throw exception");
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
        String goal = "_:length([],0)"; // invalid module prefix (unbound)
        try {
            Query.hasSolution(goal); // should throw exception
            fail(goal + " (unbound module prefix) wrongly succeeded");
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
        String goal = "f(x):length([],0)"; // invalid module prefix (compound)
        try {
            Query.hasSolution(goal); // should throw exception
            fail(goal + " (compound module prefix) wrongly succeeded");
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
        String goal = "no_such_module:no_such_predicate(0)"; // invalid module prefix (nonexistent)
        try {
            Query.hasSolution(goal); // should throw exception
            fail(goal + " (nonexistent module prefix) wrongly succeeded");
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
