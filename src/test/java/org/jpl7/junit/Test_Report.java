package org.jpl7.junit;

import org.jpl7.JPL;
import org.jpl7.Query;
import org.jpl7.Term;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.util.Map;

public class Test_Report extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main("org.jpl7.junit.Test_Report");

        // should work from static class but gives error
//        org.junit.runner.JUnitCore.main( GetSolution.class.getName()); // full name with package
    }

    /**
     * This is done at the class loading, before any test is run
     */
    @BeforeClass
    public static void setUp() {
        setUpClass();
    }

    @Rule
    public TestRule watcher = new TestWatcher() {
        protected void starting(Description description) {
            reportTest(description);
        }
    };

    @Test
    public void ReportPrologFlags() {
        Query q = new Query("current_prolog_flag(X, Y)");
        while (q.hasMoreSolutions()) {
            Map<String,Term> sol = q.nextSolution();
            reportNoise(String.format("\t Value of %s: %s",
                    sol.get("X").toString(), sol.get(("Y").toString())));
        }
    }

    @Test
    public void reportConfiguration() {
        reportNoise("\t JPL testing under the following configuration:");
        useJPLmodule();
        Term swi = Query.oneSolution("current_prolog_flag(version_data,Swi)").get("Swi");
        reportNoise("\t swipl.version = " +
                swi.arg(1) + "." + swi.arg(2) + "." + swi.arg(3));
//      This is the problematic one as it wil trigger loading library(jpl)
        reportNoise("\t swipl.syntax = " +
                Query.oneSolution("jpl_pl_syntax(Syntax)").get("Syntax"));
        reportNoise("\t swipl.home = " +
                Query.oneSolution("current_prolog_flag(home,Home)").get("Home").name());
        reportNoise("\t jpl.jar = " + JPL.version_string());
        reportNoise("\t jpl.dll = " + org.jpl7.fli.Prolog.get_c_lib_version());
        reportNoise("\t jpl.pl = " +
                Query.oneSolution("jpl_pl_lib_version(V)").get("V").name());
        reportNoise("\t java.version = " + System.getProperty("java.version"));
        reportNoise("\t java_lib_version = " + JPL.version_string());
        reportNoise("\t c_lib_version = " + org.jpl7.fli.Prolog.get_c_lib_version());
        reportNoise("\t pl_lib_version = " +
                Query.oneSolution("jpl:jpl_pl_lib_version(V)").get("V").name() + " " +
                Query.oneSolution("module_property(jpl, file(F))").get("F").name());
        reportNoise("\t os.name = " + System.getProperty("os.name"));
        reportNoise("\t os.arch = " + System.getProperty("os.arch"));
        reportNoise("\t os.version = " + System.getProperty("os.version"));
    }

    @Test
    public void report_actual_init_args() {
        String[] args = org.jpl7.fli.Prolog.get_default_init_args();
        String which;
        String s = "";
        if (args == null) {
            args = org.jpl7.fli.Prolog.get_actual_init_args();
            which = "actual";
        } else {
            which = "default";
        }
        for (int i = 0; i < args.length; i++) {
            s = s + args[i] + " ";
        }
        reportNoise(String.format("\t %s_init_args = %s", which, s));
    }
}
