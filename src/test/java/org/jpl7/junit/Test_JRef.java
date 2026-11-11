package org.jpl7.junit;

import org.jpl7.*;
import org.jpl7.Integer; // prefer to java.lang.Integer
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

public class Test_JRef extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.jpl7.JPLTestSuiteRunner.run(Test_JRef.class); // full name with package
    }

    @BeforeAll
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
        assertNull(nullObject, "JPL null Term yields a null object");
    }

    @Test
    public void testRef7() {
        Term badJRef = new Compound("hello", new Term[] { new Atom("foobar") }); // term hello(foobar)
        JPLException e = assertThrows(JPLException.class, () -> badJRef.object(),
                                      "@(foobar).object() should throw JPLException");
        assertEquals("term is neither a JRef nor a Compound representing @(null)", e.getMessage(),
                     "Exception text does not match");
    }
}
