package launching;

import static misc.supporting.Output.print;

import java.io.File;
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.util.Arrays;

import interpreting.modules.interpreter.Interpreter;
import interpreting.modules.parser.Parser;
import interpreting.program.Program;
import misc.supporting.FileManager;
import misc.supporting.Output;

/**
 * The starting point for the Interpreter.
 * 
 * Takes the execution command and possible flags, and starts the program accordingly.
 */
public abstract class Main {

	public static final Program PROGRAM = new Program();
	private static String libPath;
	private static String launchPath;
	private static boolean jStacktrace = false;
	private static int formatterLvl = 5;

	public static void main(String[] args) {
		//@formatter:off
		if (args.length < 3) {
			throw new IllegalArgumentException("The Interpreter has to get launched with following arguments:" 
				+ "\n-The library-path" + (args.length >= 1 ? "\t Was: " + args[0] : "")			
				+ "\n-The project-path" + (args.length >= 2 ? "\t Was: " + args[1] : "")
				+ "\n-An execution-command"
				+ "\n-Optional flags");
		}
		//@formatter:on
		libPath = args[0].strip();
		launchPath = args[1].strip();
		String execCommand = args[2].strip();
		String[] execFlags = Arrays.copyOfRange(args, 3, args.length);
		execute(execCommand, execFlags);
	}

	/** Executes with the specified configurations. */
	private static void execute(String execCommand, String[] execFlags) {
		switch (execCommand.toLowerCase()) {
			case "run":
				exec(execFlags, false);
				break;
			case "format":
				exec(execFlags, true);
				break;
			default:
				throw new IllegalArgumentException("Unexpected execution-command: " + execCommand);
		}
	}

	/**
	 * Starts the formatting and the execution.
	 * 
	 * @param execFlags      are optional execution-flags.
	 * @param justFormatting is true, if the program should only get formatted and not interpreted.
	 */
	private static void exec(String[] execFlags, boolean justFormatting) {
		// Set flags
		boolean force = false;
		for (String flag : execFlags) {
			flag = flag.strip();
			if ("--debug".equals(flag))
				Output.debugMode = true;
			else if ("--j-stacktrace".equals(flag))
				jStacktrace = true;
			else if (flag.matches("--formatter-lvl:\\d"))
				formatterLvl = Character.getNumericValue(flag.charAt(flag.length() - 1));
			else if ("--force-format".equals(flag))
				force = true;
			else
				throw new IllegalArgumentException("Unexpected flag for format-command: " + flag);
		}
		// Save execution
		try {
			print("Formatting...");
			Parser.parse(Path.of(libPath), findMainFile(launchPath.replace("\"", "")), force);
			if (!justFormatting) {
				print("Interpreting...");
				Interpreter.interpret();
			}
		} catch (Exception e) {
			if (jStacktrace)
				e.printStackTrace();
			else {
				System.err.println("---" + e.getClass().getSimpleName() + "---");
				System.err.println(e.getMessage());
			}
			System.exit(1);
		}
	}

	/**
	 * Finds the Main.pc file in the specified path.
	 * 
	 * @param srcPath is a top-directory of Main.pc
	 * @return the direct path of the Main.pc
	 * @throws FileNotFoundException if no main was found.
	 */
	private static Path findMainFile(String srcPath) throws FileNotFoundException {
		File f = FileManager.findFileDir(new File(srcPath), "Main.pc");
		if (f == null)
			throw new FileNotFoundException("There has to be a main-file (Main.pc) present in the src directory. (" + srcPath + ")");
		return f.toPath();
	}

	/**
	 * Returns the strength of the formatter.
	 * 
	 * Default: 5
	 */
	public static int getFormattingLvl() {
		return formatterLvl;
	}

	public static boolean showJStacktrace() {
		return jStacktrace;
	}
}
