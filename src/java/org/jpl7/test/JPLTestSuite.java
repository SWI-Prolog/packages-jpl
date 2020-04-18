package org.jpl7.test;


import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.RunWith;
import org.junit.runner.notification.Failure;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
        org.jpl7.test.junit.AtomTest.class,
        org.jpl7.test.junit.DataManagement.class,
        org.jpl7.test.junit.GetSolution.class,
        org.jpl7.test.junit.IntegerTest.class,
//        org.jpl7.test.junit.JRef.class,   // yields error when loading files.so modules
        org.jpl7.test.junit.ListTest.class,
        org.jpl7.test.junit.ModuleTest.class, // yields error when loading files.so modules
        org.jpl7.test.junit.MutualRecursion.class,    // yields error when loading files.so modules
        org.jpl7.test.junit.QueryBuilder.class,
        org.jpl7.test.junit.ReportPrologInfo.class,
        org.jpl7.test.junit.StringTest.class,
        org.jpl7.test.junit.TypesTest.class,
        org.jpl7.test.junit.Unicode.class,
        org.jpl7.test.junit.Variables.class,
        org.jpl7.test.junit.Tests.class,    // yields error when loading files.so modules
})

public class JPLTestSuite {
    // the class remains empty,
    // used only as a holder for the above annotations
    }

