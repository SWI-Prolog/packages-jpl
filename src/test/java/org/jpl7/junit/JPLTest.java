package org.jpl7.junit;

import org.jpl7.JPL;
import org.jpl7.Query;
import static org.junit.Assert.fail;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

abstract class JPLTest {
    public static final String home =
            (System.getenv("SWI_HOME_DIR") == null ? "../../build/home/"
                    : System.getenv("SWI_HOME_DIR"));
    
    public static final String startup  =
            (System.getenv("SWIPL_BOOT_FILE") == null ? String.format("%s/boot.prc", home)
                    : System.getenv("SWIPL_BOOT_FILE"));

    // Somehow Windows requires libswipl.dll (https://github.com/SWI-Prolog/packages-jpl/issues/32)
    //  but if we use that in Linux, then swipl.rc does not load the search paths correctly
    public static final String swi_exec = String.format("%s/../src/swipl", home);

    public static final String syntax =
            (System.getenv("SWIPL_SYNTAX") == null ? "modern"
                    : System.getenv("SWIPL_SYNTAX"));
    
    public static final String source_dir =
            System.getenv("SOURCE_DIR") == null ? "." : System.getenv("SOURCE_DIR");
    
    public static final String test_dir = String.format("%s/src/test/java/org/jpl7", source_dir);

    // test_jpl is the main testing file for jpl, used also for java_in_prolog
    //  it is in the original source tree (not in the build), but ../../packages/jpl = current dir so it works
    //      both for CMAKE and for development in packags/jpl (e.g., IntelliJ)
    public static final String test_jpl = String.format("%s/test_jpl.pl", source_dir);
    
    public static final boolean report =
            (System.getenv("REPORT") == null ? false // PS 12/Jul/2022 was true
                    : "true".equalsIgnoreCase(System.getenv("REPORT")));

    @BeforeClass
    public static void setUp() { // overridden where inadequate
        setUpClass();
    }

    protected static void setUpClass() {
        if (syntax.equals("traditional")) {
            JPL.setTraditional();
        }
        // CLI options for SWI: https://www.swi-prolog.org/pldoc/man?section=cmdline
        // This is how the SWI engine will be configured/set-up
        //  See this issue on how this line came up: https://github.com/SWI-Prolog/packages-jpl/issues/32
        //  swi_exec is not anymore "libswipl.dll" which was a "flag" to avoid setting up the search paths for
        //      the .so libraries in swipl.rc: now it sets up those paths always
        String init_swi_config =
                String.format("%s -x %s -F swipl --home=%s -f none -g true -q --no-signals --no-packs",
                        swi_exec, startup, home);
        // Configure the SWI engine before starting
        JPL.setDefaultInitArgs(init_swi_config.split("\\s+"));
    }

    @Rule
    public TestRule watcher = new TestWatcher() {
        protected void starting(Description description) {
            reportTest(description);
        }
    };

    // This is how the test is reported
    protected void reportTest(Description description) {
        if (report) {
            System.out.println(String.format("Starting test: %s (%s)",
                    description.getMethodName(),
                    description.getTestClass().getSimpleName()));
        }
    };
    
    // allow/suppress additional noise from successful tests
    protected void reportNoise(String msg) {
    	if (report) {
    		System.out.println(msg);
    	}
    }

    protected static void consultTestFile() {
        (new Query(String.format("consult('%s')", test_jpl))).hasSolution();
    }

    protected static void useJPLmodule() {
        Query.hasSolution("use_module(library(jpl))"); // only because we call e.g. jpl_pl_syntax/1 below
    }

    protected static void machExceptionError(Exception e, String expectedError) {
        if (e.getMessage().contains(expectedError)) {
            // OK: an appropriate exception was thrown
        } else {
            String msg = String.format("did not catch expected error '%s', received: %s ", expectedError, e.toString());
            fail(msg);
        }
    }
}
