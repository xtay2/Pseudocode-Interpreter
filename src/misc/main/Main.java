package misc.main;

import java.io.IOException;

import interpreting.modules.interpreter.Interpreter;
import interpreting.modules.parser.Parser;
import interpreting.program.Program;

public class Main {

	public static String filePath = "";

	public static final Program PROGRAM = new Program();

	public static void main(String[] args) throws IOException {
		filePath = (args.length == 1 ? args[0].substring(0, args[0].length() - 1) + "\\" : "") + "Main.pc";
//		try {
		Parser.parse();
		Interpreter.interpret();
//		} catch (InterpretingException e) {
//			System.err.println("---" + e.getClass().getSimpleName() + "---");
//			System.out.println(e.getMessage());
//		} catch (ArithmeticException e) {
//			System.err.println("---ArithmeticException---");
//			System.out.println(e.getLocalizedMessage());
//		} catch (StackOverflowError | PatternSyntaxException e) {
//			System.err.println("---StackOverflowError---");
//			System.out.println("Theres an \"infinite\" loop/recursion in the code.");
//		} catch (Exception e) {
//			e.printStackTrace();
//			throw new IllegalCodeFormatException("The Interpreter failed to read the code.");
//		}
	}
}