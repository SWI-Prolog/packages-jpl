package test3;

import java.util.Iterator;
import java.util.Map;

import org.jpl7.Query;
import org.jpl7.Term;

public class Test3 {
	public static void main(String argv[]) {
		run_tests();
	}

	static void run_tests() {
		test_0();
	}



	static void test_0() {

		Term t = Term.textToTerm("consult('test.pl')");

		Query.hasSolution(t);

		Iterator<Map<String, Term>> solutionsIter = new Query("person(X, Y, Z)");
		while (solutionsIter.hasNext()) {
			Map<String, Term> sol = solutionsIter.next();
			System.out.println("\t Solution found (now asserting to block?): " + sol.toString());
			Query.oneSolution(String.format("assertz(name_of_person(aa))", sol.get("X").toString()));
		}
		Query.hasSolution("listing(name_of_person/1)");
	}

}
