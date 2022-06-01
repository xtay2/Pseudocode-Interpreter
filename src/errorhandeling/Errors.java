package errorhandeling;

import importing.filedata.paths.DataPath;
import launching.Main;

/**
 * This class handles all thrown {@link Error}s and {@link Exception}s.
 */
public final class Errors {

	private Errors() {
		// Non instantiable
	}

	/** Gets called by {@link Main#main(String[])} */
	public static void handleError(Throwable t) {
		System.out.flush();
		switch (t) {
			case InitException ie -> handleInitException(ie);
			case PseudocodeException pce -> handlePcException(pce);
			case Error e -> handleJavaError(e);
			case Exception e -> handleJavaException(e);
			default -> handleError(new Error(t));
		}
		System.exit(1);
	}

	/** Deals with every expected {@link InitException}. */
	private static synchronized void handleInitException(InitException ie) {
		System.err.println(ie.getLocalizedMessage());
		System.err.println("A " + ie.name + " occured during the initialization.");
		printStackTrace(false, ie);
	}

	/** Deals with every expected {@link PseudocodeException}. */
	private static synchronized void handlePcException(PseudocodeException pce) {
		System.err.println(pce);
		printStackTrace(false, pce);
	}

	/** Deals with every subclass of {@link Error}. The stacktrace gets allways displayed. */
	private static synchronized void handleJavaError(Error e) {
		searchForCause(e);
		System.err.println("A major error occured inside the interpreter!");
		System.err.println("Please visit \"https://github.com/xtay2/Pseudocode/issues/new/choose\" to create a report.");
		printStackTrace(true, e);
	}

	/**
	 * Deals with every subclass of {@link Exception} except the {@link PseudocodeException}. The
	 * stacktrace is optional. The difference to {@link #handlePcException(PseudocodeException)} is,
	 * that java-exceptions don't have a attached {@link DataPath}.
	 */
	private static synchronized void handleJavaException(Exception e) {
		searchForCause(e);
		if (e.getLocalizedMessage() == null)
			handleError(new Error("Empty error message for " + e.getClass().getSimpleName() + "."));
		else {
			System.err.println("An " + e.getClass().getSimpleName() + " occured at an unknown place.");
			System.err.println(e.getLocalizedMessage());
			printStackTrace(false, e);
		}
	}

	/**
	 * If a {@link PseudocodeException} caused another exception, only the {@link PseudocodeException}
	 * gets displayed.
	 */
	private static void searchForCause(Throwable t) {
		Throwable cause = t.getCause();
		while (cause != null) {
			if (cause instanceof PseudocodeException)
				handleError(cause);
			cause = cause.getCause();
		}
	}

	/**
	 * Prints just the stacktrace (not the message) of any {@link Throwable}.
	 *
	 * @param force makes this independent from {@link Main#showJStacktrace()}
	 */
	private static synchronized void printStackTrace(boolean force, Throwable t) {
		if (force) {
			System.err.println("\nStacktrace:");
			t.printStackTrace();
		} else if (Main.showJStacktrace()) {
			System.err.println("\nStacktrace:");
			StackTraceElement[] trace = t.getStackTrace();
			for (int i = 0; i < Math.min(100, trace.length); i++)
				System.err.println("  - " + trace[i]);
		}
	}

}
