package system;

import org.jpl7.JPL;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.fli.Prolog;

public class Versions {
	public static void main(String argv[]) {

		report_versions();

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
