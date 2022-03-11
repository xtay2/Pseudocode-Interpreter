package misc.main;

import java.io.File;
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.util.Arrays;

import interpreting.modules.interpreter.Interpreter;
import interpreting.modules.parser.Parser;
import interpreting.program.Program;
import misc.helper.FileManager;

public class Main {

	public static final Program PROGRAM = new Program();

	/**
	 * Starts the interpreter
	 * 
	 * @param args has to have the length one. The first argument has to be the path of the
	 *             source-directory of the project. The second argument has to be the path of the
	 *             lib-folder.
	 */
	public static void main(String[] args) {
		try {
			if (args.length == 2) {
				Parser.parse(Path.of(args[0].strip()), findMainFile(args[1].replace("\"", "")));
				Interpreter.interpret();
			} else {
			//@formatter:off
				System.err.println("The two arguments that should get passed to the interpreter are the src and lib path.\n"
						+ "You passed " + args.length + " instead.\n" 
						+ (args.length == 1 ? "Was " : "Were ") + Arrays.toString(args));
				//@formatter:on
			}
		} catch (FileNotFoundException e) {
			System.err.println(e.getMessage());
		}
	}

	private static Path findMainFile(String srcPath) throws FileNotFoundException {
		File f = FileManager.findFileDir(new File(srcPath), "Main.pc");
		if (f == null)
			throw new FileNotFoundException("There has to be a main-file (Main.pc) present in the src directory. (" + srcPath + ")");
		return f.toPath();
	}
}