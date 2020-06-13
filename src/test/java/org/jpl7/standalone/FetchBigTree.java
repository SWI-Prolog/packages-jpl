package org.jpl7.standalone;

import org.jpl7.Query;
import org.jpl7.Term;

public class FetchBigTree {
	public static void main(String[] args) {
		// Prolog.set_default_init_args(new String[] { "libpl.dll", "-f",
		// "D:/pcm/bin/pcm.ini", "-g", "pcm_2000" });
		(new Query("consult('jpl/test/test.pl')")).oneSolution();
		Term t = new Query("p(18,T)").oneSolution().get("T");
		int i = 1;
		while (t.hasFunctor("a", 2)) {
			t = t.arg(2);
			i = i + 1;
		}
		System.err.println("got a tree of " + i + " generations");
	}
}
