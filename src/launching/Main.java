package launching;

import static misc.supporting.Output.*;

import java.io.*;
import java.nio.file.*;
import java.util.*;

import errorhandeling.*;
import importing.filedata.File;
import interpreting.modules.interpreter.*;
import interpreting.modules.parser.*;
import interpreting.program.*;
import misc.supporting.*;

/**
 * The starting point for the Interpreter.
 *
 * Takes the execution command and possible flags, and starts the program accordingly.
 */
public abstract class Main {
	
	public static final Program PROGRAM = new Program();
	
	public static String libPath, launchPath;
	private static boolean jStacktrace = false;
	private static int formatterLvl = 5;
	
	public static void main(String[] args) {
		//@formatter:off
		if (args.length < 3)
			throw new IllegalArgumentException("The Interpreter has to get launched with following arguments:"
				+ "\n-The library-path" + (args.length >= 1 ? "\t Was: " + args[0] : "")
				+ "\n-The project-path" + (args.length >= 2 ? "\t Was: " + args[1] : "")
				+ "\n-An execution-command"
				+ "\n-Optional flags");
		//@formatter:on
		libPath = args[0].strip().replace('\\', '/');
		launchPath = args[1].strip().replace('\\', '/');
		String execCommand = args[2].strip();
		String[] execFlags = Arrays.copyOfRange(args, 3, args.length);
		try {
			assert Files.exists(Path.of(libPath)) : "The passed library-path wasn't found:\n" + libPath;
			assert Files.exists(Path.of(launchPath)) : "The passed launchPath-path wasn't found:\n" + launchPath;
			execute(execCommand, execFlags);
		} catch (Throwable t) {
			Errors.handleError(t);
		}
	}
	
	/** Executes with the specified configurations. */
	private static void execute(String execCommand, String[] execFlags) {
		switch (execCommand.toLowerCase()) {
			case "run":
				exec(false, execFlags);
				break;
			case "format":
				exec(true, execFlags);
				break;
			case "project":
				if (execFlags.length != 1 || execFlags[0].isBlank())
					throw new IllegalArgumentException("The \"pseudocode project\"-command requires a name.");
				createProject(execFlags[0]);
				exec(false);
				break;
			default:
				throw new IllegalArgumentException("Unexpected execution-command: " + execCommand);
		}
	}
	
	/**
	 * Creates a new project.
	 *
	 * @param projectName is a non-null/non-empty project-name.
	 * @throws InitException if the project could not be created.
	 */
	private static void createProject(String projectName) throws InitException {
		try {
			launchPath = launchPath + "/" + projectName;
			Files.createDirectory(Path.of(launchPath));
			Path mainFilePath = Path.of(launchPath + "/" + File.MAIN_FILE + File.EXTENSION);
		// @formatter:off
		FileManager.writeFile(
				List.of("import stdlib.lang.System",
						"",
						"main {",
							"print(\"Hello World!\")",
						"}"
						), mainFilePath);
		// @formatter:on
		} catch (IOException e) {
			throw new InitException("Project \"" + projectName + "\" couldn't get created.", e);
		}
	}
	
	/**
	 * Starts the formatting and the execution.
	 *
	 * @param execFlags are optional execution-flags.
	 * @param justFormatting is true, if the program should only get formatted and not interpreted.
	 */
	private static void exec(boolean justFormatting, String... execFlags) {
		// Set flags
		boolean force = false;
		for (String flag : execFlags) {
			flag = flag.strip();
			if ("--debug".equals(flag)) {
				Output.debugMode = true;
			} else if ("--j-stacktrace".equals(flag)) {
				jStacktrace = true;
			} else if (flag.matches("--formatter-lvl:\\d")) {
				formatterLvl = Character.getNumericValue(flag.charAt(flag.length() - 1));
			} else if (justFormatting && "--force".equals(flag)) {
				force = true;
			} else
				throw new IllegalArgumentException("Unexpected flag for format-command: " + flag);
		}
		// Save execution
		print("Starting the formatter...");
		Parser.parse(force);
		if (!justFormatting) {
			print("Interpreting...");
			Interpreter.interpret();
		}
	}
	
	/**
	 * Returns the strength of the formatter.
	 *
	 * Default: 5
	 */
	public static int getFormattingLvl() { return formatterLvl; }
	
	public static boolean showJStacktrace() {
		return jStacktrace;
	}
}
