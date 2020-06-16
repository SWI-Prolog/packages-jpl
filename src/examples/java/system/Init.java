package system;

import org.jpl7.JPL;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.fli.Prolog;

import java.util.Arrays;
import java.util.Map;


public class Init {
    public static void main(String[] args) throws Exception {
        // CLI options: https://www.swi-prolog.org/pldoc/man?section=cmdline
        // https://www.swi-prolog.org/FAQ/FindResources.html


        if (System.getenv("SWI_HOME_DIR") != null ||
                System.getenv("SWI_EXEC_FILE") != null ||
                System.getenv("SWIPL_BOOT_FILE") != null) {
           String init_swi_config =
                    String.format("%s %s %s -F swipl -g true -q --no-signals --no-packs",
                            System.getenv("SWI_EXEC_FILE") == null ? "swipl" :
                                    System.getenv("SWI_EXEC_FILE"),
                            System.getenv("SWIPL_BOOT_FILE") == null ? "" :
                                    String.format("-x %s", System.getenv("SWIPL_BOOT_FILE")),
                            System.getenv("SWI_HOME_DIR") == null ? "" :
                                    String.format("--home=%s", System.getenv("SWI_HOME_DIR")));
            System.out.println(String.format("\nSWIPL initialized with: %s", init_swi_config));

            JPL.setDefaultInitArgs(init_swi_config.split("\\s+"));    // initialize SWIPL engine
        } else
            System.out.println("No explicit initialization done: no SWI_HOME_DIR, SWI_EXEC_FILE, or SWIPL_BOOT_FILE defined");

//        JPL.setTraditional();
        JPL.init();
        System.out.println("Prolog engine actual init args: " + Arrays.toString(Prolog.get_actual_init_args()));

        System.out.println();
        System.out.println("==========================================================");
        report_selected_flags();
        System.out.println("==========================================================");
        report_versions();
        System.out.println("==========================================================");
        report_all_flags();


    }

    public static void report_selected_flags() {
        System.out.println("REPORTING SELECTED FLAGS: \n");

        String prop = "[os_argv, home, executable, version, version_data, traditional, resource_database, toplevel_goal]";
        Query q = new Query(String.format("member(X, %s), current_prolog_flag(X, Y)", prop));
        while (q.hasMoreSolutions()) {
            Map<String,Term> sol = q.nextSolution();
            System.out.println(String.format("\t Value of %s: %s",   sol.get("X").toString(), sol.get(("Y"))));
        }
    }


    public static void report_all_flags() {
        System.out.println("REPORTING ALL FLAGS: \n");

        Query q = new Query("current_prolog_flag(X, Y)");
        while (q.hasMoreSolutions()) {
            Map<String,Term> sol = q.nextSolution();
            System.out.println(String.format("\t Value of %s: %s",   sol.get("X").toString(), sol.get(("Y"))));
        }
    }


    public static void report_versions() {
        System.out.println("REPORTING VERSION INFO: \n");

        System.out.println("\t swipl.home = " +
                Query.oneSolution("current_prolog_flag(home, Home)").get("Home").name());

        Term swi = Query.oneSolution("current_prolog_flag(version_data, Swi)").get("Swi");
        System.out.println("\t swipl.version = " +
                swi.arg(1) + "." + swi.arg(2) + "." + swi.arg(3));


        String pVersion = new Query("jpl_pl_lib_version(V)").oneSolution().get("V").name();
        String jVersion = JPL.version_string();
        String cVersion = Prolog.get_c_lib_version();

        System.out.println("prolog library version (jpl.pl): " + pVersion);
        System.out.println("  java library version (jpl.jar): " + jVersion);
        System.out.println("     c library version (libjpl.so): " + cVersion);

        if (pVersion.equals(jVersion) && jVersion.equals(cVersion)) {
            System.out.println("BINGO! you appear to have the same version of each library installed");
        } else {
            System.out.println("WHOOPS! you appear not to have the same version of each library installed");
        }
    }



}