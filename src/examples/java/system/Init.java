package system;

import org.jpl7.JPL;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.fli.Prolog;

import java.util.Arrays;
import java.util.Map;

import static java.lang.System.exit;


public class Init {
    public static void main(String[] args) throws Exception {
        // CLI options: https://www.swi-prolog.org/pldoc/man?section=cmdline
        // https://www.swi-prolog.org/FAQ/FindResources.html
        if (System.getenv("SWI_HOME_DIR") == null ||
                System.getenv("SWI_EXEC_FILE") == null ||
                System.getenv("SWIPL_BOOT_FILE") == null) {
            System.out.println("You have to define SWI_HOME_DIR, SWI_EXEC_FILE and SWIPL_BOOT_FILE to run this example");
            exit(1);
        }


        String init_swi_config =
                String.format("%s -x %s --home=%s  -g true -q --no-signals --no-packs",
                        System.getenv("SWI_EXEC_FILE"),
                        System.getenv("SWIPL_BOOT_FILE"),
                        System.getenv("SWI_HOME_DIR")); // can be just guessed from executable

        System.out.println(init_swi_config);
        JPL.setDefaultInitArgs(init_swi_config.split("\\s+"));	// initialize SWIPL engine
        JPL.setTraditional();
        JPL.init();

        System.out.println("Prolog engine actual init args: " + Arrays.toString(Prolog.get_actual_init_args()));


        String prop = "[os_argv, home, executable, version, traditional, resource_database, c_libplso, toplevel_goal]";
//        Query q = new Query(String.format("member(X, %s), current_prolog_flag(X, Y)", prop));
        Query q = new Query("current_prolog_flag(X, Y)");
        q.open();
        while (q.hasMoreSolutions()) {
            Map<String,Term> sol = q.nextSolution();
            System.out.println(String.format("\t Value of %s: %s",   sol.get("X").toString(), sol.get(("Y").toString())));
        }
        q.close();
    }



}