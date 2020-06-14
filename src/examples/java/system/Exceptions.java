package system;

import org.jpl7.PrologException;
import org.jpl7.Query;

public class Exceptions {
	public static void main(String argv[]) {
		// currently, SWI-Prolog's default args are suited to interactive use with an attached console,
		// not to embedded use like this, so we override them before they are used
		// (by JPL, when it necessarily initialises Prolog when .hasSolution() is first called)
		// Prolog.set_default_init_args(new String[] { "libpl.dll", "-f", "none", "-g", "set_prolog_flag(debug_on_error,false)", "-q" });
		System.out.print("calling\n\n");
		System.out.print("?- X is Y.\n\n");
		System.out.print("in Prolog to force a Prolog 'instantiation_error' exception,\n");
		System.out.print("which should be returned via Java as an uncaught org.jpl7.PrologException in thread \"main\".\n\n");
		try {
			Query.hasSolution("X is Y");
		} catch (PrologException e) {
			System.out.println("BINGO! The following exception was thrown: \n\t" + e.getMessage());
		}
	}
}
