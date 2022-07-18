package org.jpl7.junit;

import org.jpl7.*;
import org.jpl7.Integer; // prefer to java.lang.Integer
import static org.junit.Assert.*;
import org.junit.BeforeClass;
import org.junit.Test;

public class Test_JRef extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main(Test_JRef.class.getName()); // full name with package
    }

    @BeforeClass
    public static void setUp() {
        setUpClass();
        consultTestFile();  // load the test_jpl.pl file
    }

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
        assertEquals("xyz", sb.toString());
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
        assertEquals(((StringBuffer) (a.object())).toString(), token);
    }

    @Test
    public void testRef6() {
        Term nullJRef = JPL.newJRef(null);
        Object nullObject = nullJRef.object();
        assertNull("JPL null Term yields a null object", nullObject);
    }

    @Test(expected = JPLException.class)
    public void testRef7() {
        Term badJRef = new Compound("hello", new Term[] { new Atom("foobar") }); // term hello(foobar)
        try {
            badJRef.object(); // should throw exception
            fail("@(foobar).object() should thrown JPLException"); // shouldn't get to here
        } catch (JPLException e) { // expected exception class
            // From JUNIT 4.5 would be good to use assertThat(e.getMessage(), is("term is neither a JRef nor a Compound representing @(null)"));
            // check http://junit.sourceforge.net/doc/ReleaseNotes4.4.html for assertThat
            assertEquals("Exception text does not match", "term is neither a JRef nor a Compound representing @(null)", e.getMessage());
            throw e;    // All good, keep throwing it so that the @Test directive catches it
        }
    }
}
