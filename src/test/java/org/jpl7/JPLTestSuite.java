package org.jpl7;


import org.jpl7.junit.*;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
        Test_Atom.class,
        Test_Data.class,
        Test_Dict.class,
        Test_Equals.class,
        Test_Exceptions.class,
        Test_GetSolution.class,
        Test_Integer.class,
        Test_List.class,
        Test_JRef.class,
        Test_Module.class,
        Test_MutualRecursion.class,
        Test_QueryBuilder.class,
        Test_Rational.class,
        Test_Report.class,
        Test_String.class,
        Test_Types.class,
        Test_Unicode.class,
        Test_Variables.class,
        Tests.class,
})

public class JPLTestSuite {
    // the class remains empty,
    // used only as a holder for the above annotations
    }

