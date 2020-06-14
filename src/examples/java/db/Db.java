package db;

import java.util.Iterator;
import java.util.Map;

import org.jpl7.*;


public class Db {
    public static void main(String[] args) {
        Term t = Term.textToTerm("consult('db.pl')");
        Query.hasSolution(t);

        Iterator<Map<String, Term>> solutionsIter = new Query("person(X, Y, Z)");
        while (solutionsIter.hasNext()) {
            Map<String, Term> sol = solutionsIter.next();
            System.out.println("\t Solution found (now asserting to block?): " + sol.toString());
            Query.oneSolution(String.format("assertz(name_of_person(%s))", sol.get("X").toString()));
        }
        Query.hasSolution("listing(name_of_person/1)");
    }
}