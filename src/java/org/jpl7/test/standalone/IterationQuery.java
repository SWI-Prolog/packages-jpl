package org.jpl7.test.standalone;

import org.jpl7.*;

import java.util.Map;
import java.util.NoSuchElementException;

public class IterationQuery {
	public static void main(String argv[]) {
		// JPL.setTraditional();
		//
		Query.hasSolution("use_module(library(jpl))"); // only because we call e.g. jpl_pl_syntax/1 below
		Term swi = Query.oneSolution("current_prolog_flag(version_data,Swi)").get("Swi");
		System.out.println("swipl.version = " + swi.arg(1) + "." + swi.arg(2) + "." + swi.arg(3));
		System.out.println("swipl.syntax = " + Query.oneSolution("jpl_pl_syntax(Syntax)").get("Syntax"));
		System.out.println("swipl.home = " + Query.oneSolution("current_prolog_flag(home,Home)").get("Home").name());
		System.out.println("jpl.jar = " + JPL.version_string());
		System.out.println("jpl.dll = " + org.jpl7.fli.Prolog.get_c_lib_version());
		System.out.println("jpl.pl = " + Query.oneSolution("jpl_pl_lib_version(V)").get("V").name());

        System.out.println("########################################################");

        Query q;
        Map<String, Term> sol;


        // First we try the JPL interface hasMoreSolutions() and nextSolution()
        q = new Query("between(1,3,N)");
        sol = q.nextSolution();
        System.out.println(String.format("Solution found (without hasMoreSolution() call): %s", sol.toString()));
		while (q.hasMoreSolutions()) {
		    sol = q.nextSolution();
		    System.out.println(String.format("\tSolution found: %s", sol.toString()));
        }

        System.out.println(String.format("Do we have more solutions available? %B", q.hasMoreSolutions()));
        System.out.println(String.format("Is the query open? %B", q.isOpen()));

        // Next we reset and try the iterator hasNext() and next() interface
        q.reset();
        System.out.println(String.format("Is the query open AFTER RESET? %B", q.isOpen()));
        System.out.println(String.format("Do we have more solutions available AFTER RESET? %B", q.hasMoreSolutions()));
        sol = q.nextSolution();
        System.out.println(String.format("Solution found (without hasNext() call): %s", sol.toString()));
        while (q.hasNext()) {
            sol = q.next();
            System.out.println(String.format("\t Solution found: %s", sol.toString()));
        }
        System.out.println(String.format("Is the query open now? %B", q.isOpen()));
        System.out.println(String.format("Do we have more solutions available now? %B", q.hasMoreSolutions()));
        try {
            sol = q.next();
        } catch (NoSuchElementException e) {
            System.out.println("All good, exception caught (trying to advance iterator, but no next): " + e.getMessage());
        }



    }
}
