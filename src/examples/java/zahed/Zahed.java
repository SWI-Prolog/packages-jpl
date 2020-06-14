package zahed;

import java.util.Map;

import org.jpl7.*;

public class Zahed {
	public static void main(java.lang.String argv[]) {
		System.out.println("starting...");
		Compound goal1 = new Compound("consult", new Term[] { new Atom("zahed.pl") });
		Query q1 = new Query(goal1);
		if (!q1.hasSolution()) {
			System.out.println("consult('zahed.pl') failed");
			return;
		}

		// creates term t(v, [], a)
		Term t2 = new Compound("t", new Term[] { new Atom("v"), JPL.LIST_NIL, new Atom("a") });

		// list2 = [t(v, [], a)]
		Compound list2 = new Compound(JPL.LIST_PAIR, new Term[] { t2, JPL.LIST_NIL });

		// creates term t(c, q, [])
		Compound t1 = new Compound("t", new Term[] { new Atom("c"), new Atom("q"), JPL.LIST_NIL });

		// list1 = [t(v, [], a), t(c, q , [])]
		Compound list1 = new Compound(JPL.LIST_PAIR, new Term[] { t1, list2 });

		Variable answer = new Variable("A");

		// goal2 = gen([t(v, [], a), t(c, q , [])], A)
		Compound goal2 = new Compound("gen", new Term[] { list1, answer});

		System.out.println("The query constructed is: " + goal2.toString());
		Query q2 = new Query(goal2);
		Map<String, Term> solution = q2.oneSolution();
		if (solution == null) {
			System.out.println("failed");
		} else {
			System.out.println(solution.get("A").toString());
		}
		System.out.println("finished");
	}
}
