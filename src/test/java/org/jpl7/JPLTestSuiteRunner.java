package org.jpl7;

import static org.junit.platform.engine.discovery.DiscoverySelectors.selectClass;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import org.jpl7.junit.*;
import org.junit.platform.engine.DiscoverySelector;
import org.junit.platform.launcher.Launcher;
import org.junit.platform.launcher.LauncherDiscoveryRequest;
import org.junit.platform.launcher.core.LauncherDiscoveryRequestBuilder;
import org.junit.platform.launcher.core.LauncherFactory;
import org.junit.platform.launcher.listeners.SummaryGeneratingListener;
import org.junit.platform.launcher.listeners.TestExecutionSummary;

/**
 * Run the JPL JUnit (Jupiter) tests using the JUnit Platform launcher.
 * Works with JUnit 5 and JUnit 6.  The process exit status is 0 if all
 * tests succeeded and 1 otherwise.
 */

public class JPLTestSuiteRunner {

    public static final Class<?>[] TESTS = {
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
        Tests.class
    };

    public static void main(String[] args) {
        System.exit(run(TESTS) ? 0 : 1);
    }

    /**
     * Run the tests in the given classes and print a summary.
     * @return true if all tests succeeded
     */
    public static boolean run(Class<?>... classes) {
        List<DiscoverySelector> selectors = new ArrayList<>();
        for (Class<?> c : classes) {
            selectors.add(selectClass(c));
        }
        LauncherDiscoveryRequest request =
            LauncherDiscoveryRequestBuilder.request().selectors(selectors).build();
        Launcher launcher = LauncherFactory.create();
        SummaryGeneratingListener listener = new SummaryGeneratingListener();
        launcher.execute(request, listener);

        TestExecutionSummary summary = listener.getSummary();
        PrintWriter out = new PrintWriter(System.out);
        summary.printFailuresTo(out);
        out.flush();

        long failed    = summary.getTotalFailureCount();
        long succeeded = summary.getTestsSucceededCount();
        if (failed == 0) {
            System.out.println("++ OK ++");
            System.out.println("all " + succeeded + " tests succeeded");
        } else {
            System.out.println("-- FAIL --");
            System.out.println("only " + succeeded + " tests succeeded");
            System.out.println("failed: " + failed);
        }
        if (summary.getTestsSkippedCount() > 0) {
            System.out.println("ignored: " + summary.getTestsSkippedCount());
        }

        return failed == 0;
    }
}
