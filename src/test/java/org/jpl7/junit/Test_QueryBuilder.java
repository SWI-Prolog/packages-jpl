package org.jpl7.junit;

import org.jpl7.*;
import org.jpl7.Float; // prefer to java.lang.Float
import org.jpl7.Integer; // prefer to java.lang.Integer
import static org.junit.Assert.*;
import org.junit.Test;

public class Test_QueryBuilder extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main(Test_QueryBuilder.class.getName()); // full name with package
    }

    @Test
    public void testTerm1() {
        Term args = Term.textToTerm("[1,2,3,4,5]");
        Term t = new Compound("member", new Term[] { new Integer(1), args } );
        Query q = new Query(t);
        assertTrue("Query should have succeded, but it did not!", q.hasSolution());
    }

    @Test
    public void testTermErr1() {
        try {
            new Query(new Float(1.23)); // unsuitable goal (neither Atom nor Integer)
            fail("Query should have given JPLException");
        } catch (JPLException e) { // expect "a Query's goal must be an Atom or Compound (not a Float)"
            // all good!
            reportNoise("\t" + e.getMessage());
        } catch (Exception e) {
            fail("Query should have given JPLException");
        }
    }

    @Test
    public void testString1() {
        Query q = new Query("member(1, [1,2,3])");
        assertTrue("Query should have succeded, but it did not!", q.hasSolution());
    }

    @Test
    public void testString2() {
        Query q = new Query("atom(?)", new Atom("hello"));
        assertTrue("Query should have succeded, but it did not!", q.hasSolution());
    }

    @Test
    public void testString3() {
        Term[] args = new Term[] { new Integer(1), Term.textToTerm("[1,2,3,4,5]") };
        Query q = new Query("member(?, ?)", args);
        assertTrue("Query should have succeded, but it did not!", q.hasSolution());
    }

    @Test
    public void testString4() {
        Term[] args = new Term[] { new Integer(1), Term.textToTerm("[1,2,3,4,5]") };
        Query q = new Query("member", args);
        assertTrue("Query should have succeded, but it did not!", q.hasSolution());
    }

    @Test
    public void testString5() {
        Query q = new Query("atom", new Atom("hello"));
        assertTrue("Query should have succeded, but it did not!", q.hasSolution());
    }

    @Test
    public void testStringErr1() {
        try {
            new Query("112"); // unsuitable goal (neither Atom nor Integer)
            fail("Query should have given JPLException");
        } catch (JPLException e) { // expect "a Query's goal must be an Atom or Compound (not an Integer)"
            // all good!
            reportNoise("\t" + e.getMessage());
         } catch (Exception e) {
            fail("Query should have given JPLException");
        }
    }

    // Query text is not a proper term
    @Test
    public void testStringErr2() {
        try {
            new Query("112(sas,23"); // unsuitable goal (not a Term)
            fail("Query should have given PrologException: malformed query");
        } catch (PrologException e) { // expect it to match error(syntax_error(_), _)
            // all good!
            reportNoise("\t" + e.getMessage());
        } catch (JPLException e) {
            fail("Should have been caught before because JPLException is a subclass of PrologException");
        } catch (Exception e) {
            fail("Query should have given JPLException");
        }
    }

    @Test
    public void testStringErr3() {
        Term[] args = new Term[] { new Integer(1), Term.textToTerm("[1,2,3,4,5]") };
        try {
            new Query("member(?, ?, ?)", args); // too few args
            fail("Query should have given JPLException");
        } catch (JPLException e) { // expect "fewer actual params than formal params"
            // all good!
            reportNoise("\t" + e.getMessage());
        } catch (Exception e) {
            fail("Query should have given JPLException");
        }
    }

    @Test
    public void testStringErr4() {
        Term[] args = new Term[] { new Integer(1), Term.textToTerm("[1,2,3,4,5]") };
        try {
            new Query("member(?)", args); // too many args
            fail("Query should have given JPLException");
        } catch (JPLException e) { // expect "more actual params than formal"
            // all good!
            reportNoise("\t" + e.getMessage()); // spurious success noise
        } catch (Exception e) {
            fail("Query should have given JPLException");
        }
    }
}
