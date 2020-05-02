package org.jpl7.test;


import org.jpl7.test.junit.Test_QueryBuilder;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
        org.jpl7.test.junit.AtomTest.class,
        org.jpl7.test.junit.DataManagement.class,
        org.jpl7.test.junit.GetSolution.class,
        org.jpl7.test.junit.IntegerTest.class,
        org.jpl7.test.junit.ListTest.class,
        org.jpl7.test.junit.JRef.class,
        org.jpl7.test.junit.ModuleTest.class,
        org.jpl7.test.junit.MutualRecursion.class,
        Test_QueryBuilder.class,
        org.jpl7.test.junit.RationalTest.class,
        org.jpl7.test.junit.ReportPrologInfo.class,
        org.jpl7.test.junit.StringTest.class,
        org.jpl7.test.junit.TypesTest.class,
        org.jpl7.test.junit.Unicode.class,
        org.jpl7.test.junit.Variables.class,
        org.jpl7.test.junit.Tests.class,
        org.jpl7.test.junit.Test_Equals.class,
})

public class JPLTestSuite {
    // the class remains empty,
    // used only as a holder for the above annotations
    }

