package org.jpl7.junit;

import java.util.Map;
import org.jpl7.*;
import org.junit.Test;

public class Test_Report extends JPLTest {

    public static void main(String argv[]) {
        // To be able to call it from CLI without IDE (e.g., by CMAKE)
        org.junit.runner.JUnitCore.main(Test_Report.class.getName()); // full name with package
    }

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
