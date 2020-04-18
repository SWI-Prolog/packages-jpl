package org.jpl7.test.junit;

import org.jpl7.*;
import org.jpl7.Integer;
import org.jpl7.fli.Prolog;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import static org.junit.Assert.*;


public class JRef extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.test.junit.JRef");

        // should work from static class but gives error
//        org.junit.runner.JUnitCore.main( GetSolution.class.getName()); // full name with package
    }

    /**
     * This is done at the class loading, before any test is run
     */
    @BeforeClass
    public static void setUp() {
        setUpClass();

        //  Query q_load_test_jpl = new Query("consult", new Term[] { new Atom(test_jpl) }))

        consultTestFile();  // load the test_jpl.pl file
        useJPLmodule();     // consult the jpl.pl module


        // The above two give error when basically doing this:
//                 Query q = new Query("use_foreign_library(foreign(files), install_files)");
//                q.hasSolution();
        /**
         * We get many errors when loading the files above
         *
         * The only way to make it work is to have libjpl.so and files.so in
         * build/home/lib, which does not exist in cmake local devel!
         *
         *  1- create symb links in build/home/lib/libjpl.so to ../../packages/jpl/libjpl.so
         *  2- create symb links in build/home/lib/files.so to ../../packages/clib/files.o
         *
         *  Somehow then those files will be loaded. Even if I put ../../packages/*** in the
         *  LD_LIBRARY_PATH, the test won't find them!
         *
         *  We also need to search the main SWI lib  in ../../build/src/ or
         *
         *  LD_LIBRARY_PATH=../../build/packages/jpl:../../build/src/
         *  SWI_HOME_DIR=../../build/home
         *  SWIPL_BOOT_FILE=../../build/home/boot.prc;
         *  LD_PRELOAD=libswipl.so
         */

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
    public void testJRef1() {
        int i = 76543;
        Integer I = new Integer(i);
        Query q = new Query("jpl_call(?,intValue,[],I2)", new Term[] { JPL.newJRef(I) });
        Term I2 = q.oneSolution().get("I2");
        assertTrue(I2.isInteger() && I2.intValue() == i);
    }

    @Test
    public void testJRef2() {
        int i = 76543;
        Integer I = new Integer(i);
        Query q = new Query("jpl_call(?,intValue,[],I2)", JPL.newJRef(I));
        Term I2 = q.oneSolution().get("I2");
        assertTrue(I2.isInteger() && I2.intValue() == i);
    }

    @Test
    public void testJRef3() {
        StringBuffer sb = new StringBuffer();
        Query.oneSolution("jpl_call(?,append,['xyz'],_)", new Term[] { JPL.newJRef(sb) });
        assertTrue(sb.toString().equals("xyz"));
    }

    @Test
    public void testJRef4() {
        Term jrefSB = Query.oneSolution("jpl_new('java.lang.StringBuffer',['abc'],SB)").get("SB");
        assertTrue(jrefSB.isJRef() && ((StringBuffer) jrefSB.object()).toString().equals("abc"));
    }

    @Test
    public void testJRef5() {
        String token = "foobar345";
        Term a = Query.oneSolution("jpl_new('java.lang.StringBuffer',[?],A)", new Term[] { new Atom(token) }).get("A");
        assertTrue(((java.lang.StringBuffer) (a.object())).toString().equals(token));
    }



    @Test
    public void testRef6() {
        Term nullJRef = JPL.newJRef(null);
        Object nullObject = nullJRef.object();
        assertNull("JPL null Term yields a null object", nullObject);
    }


    @Test
    public void testRef7() {
        Term badJRef = new Compound("hello", new Term[] { new Atom("foobar") }); // term hello(foobar)
        try {
            badJRef.object(); // should throw exception
            fail("@(foobar).object() should thrown JPLException"); // shouldn't get to here
        } catch (JPLException e) { // expected exception class
            if (e.getMessage().endsWith("term is not a JRef")) {
                // OK: an appropriate exception was thrown
            } else {
                fail("hello(foobar).object() threw wrong JPLException: " + e);
            }
        } catch (Exception e) {
            fail("hello(foobar).object() threw wrong exception class: " + e);
        }
    }


}
