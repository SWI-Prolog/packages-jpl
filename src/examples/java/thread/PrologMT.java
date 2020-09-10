package thread;

import java.util.Map;
import java.util.ArrayList;

import org.jpl7.Compound;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.fli.Prolog;

public class PrologMT extends Thread {
	int id; // client thread id
	int chats = 1;

	PrologMT(int i) {
		this.id = i;
	}

	public static int pid() {
		Map<String, Term> solution;
		Query q = new Query("current_prolog_flag(pid, PID)");
		solution = q.oneSolution();
		return solution.get("PID").intValue();
	}

	public static void main(String argv[]) {
		int max = 20;
		int runs = 1;
		ArrayList<Thread> threads = new ArrayList<Thread>();

		// Provide ssh server in the process.  Connect using ssh -p 2020 localhost
		if ( new Query("exists_source(library(ssh_server))").hasSolution() ) {
			new Query("use_module(library(ssh_server))").hasSolution();
			new Query("ssh_server(2020)").hasSolution();
		}

		new Query("consult(program)").hasSolution();

		if ( argv.length > 0 )
		  max = Integer.parseInt(argv[0]);
		if ( argv.length > 1 )
		  runs = Integer.parseInt(argv[1]);

		for (int r = 0; r < runs; r++ ) {
			  long t0 = System.currentTimeMillis();
			  for (int i = 0; i < max; i++) {
				  Thread t = new PrologMT(i);
				  t.start();
				  threads.add(t);
			  }
			  long t1 = System.currentTimeMillis()-t0;
			  System.out.println("Created "+max+" threads in "+t1+"ms; waiting (pid="+pid()+") ...");
			  for (int i = 0; i < threads.size(); i++)
			  { try {
				  threads.get(i).join();
			    } catch(Exception e) {
				  System.out.println("Join failed");
			    }
			  }
			  long t2 = System.currentTimeMillis()-t0;
			  System.out.println("All done in "+t2+"ms");
		}
	}

/** Run a Java thread with a Prolog engine attached during the life time
 *  of the thread.  The thread must call Prolog.destroy_engine() before
 *  terminating.  Eventually we would like to make that optional, but I
 *  have not yet found a way to get notified in time. We can get a
 *  notification using the destruction function of a pthread local key,
 *  but that is called after the thread died and we can no longer do
 *  Prolog's thread cleanup magic.  Currently we use this to print
 *  a message telling you forgot to call Prolog.destroy_engine()
 */

	public void run() {
		Map<String, Term> solution;

		Prolog.create_engine();
		//new Query("welcome").hasSolution();
		//new Query("time(rtest_chats("+chats+"))").hasSolution();
		new Query("rtest_chats("+chats+")").hasSolution();
		Prolog.destroy_engine();
		//System.out.println("Done");
	}
}
