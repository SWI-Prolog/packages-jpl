package org.jpl7;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

public class JPLTestSuiteRunner {

    public static void main(String[] args) {
        Result result = JUnitCore.runClasses(JPLTestSuite.class);

        for (Failure failure : result.getFailures()) {
            System.out.println(failure.toString());
            failure.getException().printStackTrace();
        }

//        System.out.println("********* Test successful? " + result.wasSuccessful());
        if (result.wasSuccessful()) {
        	System.out.println("++ OK ++");
        	System.out.println("all " + result.getRunCount() + " tests succeeded");
        } else {
        	System.out.println("-- FAIL --");
        	System.out.println("only " + result.getRunCount() + " tests succeeded");
        }
        if (result.getFailureCount() > 0) {
        	System.out.println("failed: " + result.getFailureCount());
        }
        if (result.getIgnoreCount() > 0) {
        	System.out.println("ignored: " + result.getIgnoreCount());
        }
    }
}


