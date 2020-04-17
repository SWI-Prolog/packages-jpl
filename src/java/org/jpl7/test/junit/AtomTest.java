package org.jpl7.test.junit;

import org.jpl7.Integer;
import org.jpl7.*;
import org.jpl7.fli.Prolog;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import static org.junit.Assert.*;


public class AtomTest {
    public static final String startup =
            (System.getenv("SWIPL_BOOT_FILE") == null ? "../../src/swipl.prc"
                    : System.getenv("SWIPL_BOOT_FILE"));
    public static final String test_jpl =
            (System.getenv("TEST_JPL") == null ? "test_jpl.pl"
                    : System.getenv("TEST_JPL"));
    public static final String syntax =
            (System.getenv("SWIPL_SYNTAX") == null ? "modern"
                    : System.getenv("SWIPL_SYNTAX"));
    public static final String home =
            (System.getenv("SWI_HOME_DIR") == null ? "../.."
                    : System.getenv("SWI_HOME_DIR"));



	public static void main(String argv[]) {
    }

    ///////////////////////////////////////////////////////////////////////////////
    // TESTING CONFIGURATION
    ///////////////////////////////////////////////////////////////////////////////

    /**
     * This is done at the class loading, before any test is run
     */
    @BeforeClass
    public static void setUp() {
        if (syntax.equals("traditional")) {
            JPL.setTraditional();
            Prolog.set_default_init_args(new String[] {
//					"libswipl.dll", "-x", startup, "-f", "none",
                    "libswipl.dll", "-f", "none",
                    "-g", "true", "--traditional", "-q",
                    "--home="+home, "--no-signals", "--no-packs" });
        } else {
            Prolog.set_default_init_args(new String[] {
//					"libswipl.dll", "-x", startup, "-f", "none",
                    "libswipl.dll", "-f", "none",
                    "-g", "true", "-q",
                    "--home="+home, "--no-signals", "--no-packs" });
        }
    }

    @Rule
    public TestRule watcher = new TestWatcher() {
        protected void starting(Description description) {
//            logger.info("{} being run...", description.getMethodName());

            System.out.println("Starting test: " + description.getMethodName());
        }
    };




    ///////////////////////////////////////////////////////////////////////////////
    // SUPPORTING CODE
    ///////////////////////////////////////////////////////////////////////////////



    ///////////////////////////////////////////////////////////////////////////////
    // TESTS
    ///////////////////////////////////////////////////////////////////////////////


    @Test
    public void testAtom1() {
        assertTrue("new Atom(\"3 3\")" + (new Atom("3 3")).toString(), true);
    }

    @Test
    public void testAtomName1() {
        String name = "fred";
        Atom a = new Atom(name);
        assertEquals("an Atom's name is that with which it was created", a.name(), name);
    }

    @Test
    public void testAtomName2() {
        String name = "ha ha";
        Atom a = new Atom(name);
        assertEquals("an Atom's name is that with which it was created", a.name(), name);
    }

    @Test
    public void testAtomName3() {
        String name = "3";
        Atom a = new Atom(name);
        assertEquals("an Atom's name is that with which it was created", a.name(), name);
    }

    @Test
    public void testAtomToString1() {
        String name = "fred";
        String toString = "fred";
        Atom a = new Atom(name);
        assertEquals("an Atom's .toString() value is quoted iff appropriate", a.toString(), toString);
    }

    @Test
    public void testAtomToString2() {
        String name = "ha ha";
        String toString = "'ha ha'";
        Atom a = new Atom(name);
        assertEquals("an Atom's .toString() value is quoted iff appropriate", a.toString(), toString);
    }

    @Test
    public void testAtomToString3() {
        String name = "3";
        String toString = "'3'";
        Atom a = new Atom(name);
        assertEquals("an Atom's .toString() value is quoted iff appropriate", a.toString(), toString);
    }

    @Test
    public void testAtomArity() {
        Atom a = new Atom("willy");
        assertEquals("an Atom has arity zero", a.arity(), 0);
    }

    @Test
    public void testAtomEquality1() {
        String name = "fred";
        Atom a1 = new Atom(name);
        Atom a2 = new Atom(name);
        assertEquals("two Atoms created with the same name are equal", a1, a2);
    }

    @Test
    public void testAtomIdentity() { // how could this fail?!
        String name = "fred";
        Atom a1 = new Atom(name);
        Atom a2 = new Atom(name);
        assertNotSame("two Atoms created with the same name are not identical", a1, a2);
    }

    @Test
    public void testAtomHasFunctorNameZero() {
        String name = "sam";
        Atom a = new Atom(name);
        assertTrue("a text atom has a functor whose name is the name of the atom, and whose arity is zero",
                a.hasFunctor(name, 0));
    }

    @Test
    public void testAtomHasFunctorWrongName() {
        assertFalse("an Atom does not have a functor whose name is other than that with which the Atom was created",
                new Atom("wally").hasFunctor("poo", 0));
    }

    @Test
    public void testAtomHasFunctorWrongArity() {
        String name = "ted";
        assertFalse("an Atom does not have a functor whose arity is other than zero",
                new Atom(name).hasFunctor(name, 1));
    }


    @Test
    public void testAtomEquality2() {
        Atom a = new Atom("a");
        assertTrue("two references to an Atom are equal by .equals()", a.equals(a));
    }

    public void testAtomEquality3() {
        assertTrue("two distinct, same-named Atoms are equal by .equals()", (new Atom("a")).equals(new Atom("a")));
    }


}
