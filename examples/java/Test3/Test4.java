import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;

import org.jpl7.*;
import org.jpl7.fli.Prolog;


public class Test4 {


    public static void main(String[] args) throws Exception {
        // CLI options: https://www.swi-prolog.org/pldoc/man?section=cmdline
        // https://www.swi-prolog.org/FAQ/FindResources.html

        String init_swi_config =
                String.format("%s -x %s -F swipl --home=%s  -f none -g true -q --no-signals --no-packs",
                        System.getenv("SWI_EXEC_FILE"),
                        System.getenv("SWIPL_BOOT_FILE"),
                        System.getenv("SWI_HOME_DIR"));
        System.out.println(init_swi_config);
        JPL.setTraditional();


//        init_swi_config =
//                String.format("xxx -q",
//                        System.getenv("SWI_EXEC_FILE"));
//
//        init_swi_config =
//                String.format("--home=%s -f none -g true -q --no-signals --no-packs",
//                        System.getenv("SWI_HOME_DIR"));
        JPL.setDefaultInitArgs(init_swi_config.split("\\s+"));
        JPL.init();

        System.out.println(Arrays.toString(Prolog.get_actual_init_args()));


        test_0();
        test_1();
    }



    static void test_0() {
        StringBuffer sb = new StringBuffer();
        String prop = "[os_argv, home, executable, version, traditional, resource_database, c_libplso, toplevel_goal]";
//        Query q = new Query(String.format("member(X, %s), current_prolog_flag(X, Y)", prop));
        Query q = new Query("current_prolog_flag(X, Y)");
        System.out.println("ssssssssssssss");
        q.open();
        while (q.hasMoreSolutions()) {
            Map<String,Term> sol = q.nextSolution();
            System.out.println(String.format("\t Value of %s: %s",   sol.get("X").toString(), sol.get(("Y").toString())));
        }
        q.close();
    }


    static void test_1() {

        Term t = Term.textToTerm("consult('test.pl')");


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