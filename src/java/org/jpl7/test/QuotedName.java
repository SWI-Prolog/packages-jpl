package org.jpl7.test;

import java.util.Map;

import org.jpl7.*;

public class QuotedName {
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

        System.out.println("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");


        String text_t;
        String name;
        Term t;

		if (Query.hasSolution("consult('src/java/org/jpl7/test/quoted_name.pl')")) {
            System.out.println("Loaded...........");
        }

        name = "org.jpl7.PrologException: PrologException: error(existence_error(procedure, '/'(pepe, 1)), context(':'(system, '/'('$c_call_prolog', 0)), _0))";
//        name = "existence_error(procedure, '/'(pepe, 1))";
//        name = "'/'(pepe, 1)";
        name = "'$c_call_prolog'";
        t = Util.textToTerm(name);
        text_t = t.toString();
        System.out.println(text_t);


        t = new Compound("error", new Term[] {
                new Atom("existence_error"),
                new Atom("context")});

        text_t = t.toString();
        System.out.println(text_t);


        name = "jpl:test";
        t = Util.textToTerm(name);
        text_t = t.toString();
        System.out.println(text_t);


        t = new Compound(":", new Term[] {
                 new Atom("jpl"),
                 new Atom("test")});

        text_t = t.toString();
        System.out.println(text_t);

        try {
            if (Query.hasSolution(t)) {};
        } catch (PrologException e) {
            System.out.println("Exception caught: " + e.getMessage());
        }





//        name = "pras(chrozi)";
//      t = new Compound(":", new Term[] {
//                 new Atom("jpl"),
//                 new Compound("quoted_name", new Term[] {
//                        new Atom(name),
//                        new Variable("S")
//                    })
//        });
//
//      t = Util.textToTerm(String.format("jpl:(quoted_name(%s, S))", name));

//        t = Util.textToTerm(String.format("jpl:test", name));

//        Term t =                new Compound("quoted_name", new Term[] {
//                        new Atom(name),
//                        new Variable("S")
//                });

//        System.out.println("#################################################");
//        Map<String, Term> sol = new Query(t).oneSolution();
//        System.out.println("111111111111111111111111111111111111111111111111111111");

//        String name2 = ((Atom) sol.get("S")).name();


//        System.out.println("Converted into: " + name2);
//        System.out.println(JPL.quotedName(name));
//        System.out.println("-----------------------------------------------------");
//
//		System.out.println("HERE WE GOOOOOOOOOOOOOOOOOOOOOOOOOOOo");
//		String t1 = "consult('src/java/org/jpl7/test/family.pl')";
//		System.out.println(t1 + " " + (Query.hasSolution(t1) ? "succeeded" : "failed"));
		//
//		System.out.println("=================================================");
//		String t2 = "child_of2(joe, ralph)";
//		boolean x = Query.hasSolution(t2);
//		System.out.println(t2 + " is " + (x ? "provable" : "not provable"));
//		//
//		String t4 = "child_of(joe, X)";
//		System.out.println("first solution of " + t4 + ": X = " + Query.oneSolution(t4).get("X"));
	}
}
