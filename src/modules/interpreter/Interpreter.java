package modules.interpreter;

import static helper.Output.UNDERLINE;
import static helper.Output.print;

import expressions.abstractions.GlobalScope;
import expressions.abstractions.MainExpression;
import expressions.main.functions.MainFunction;
import expressions.main.functions.Returnable;
import expressions.main.statements.ReturnStatement;
import expressions.normal.containers.Variable;
import expressions.possible.assigning.Declaration;
import main.Main;
import modules.parser.program.ProgramLine;

public final class Interpreter {
	/**
	 *
	 * Registeres every {@link Variable} and {@link Returnable} and starts the interpreting-process by
	 * calling the {@link MainFunction}.
	 * 
	 * @param program is the program that gets interpreted.
	 * 
	 * @see Main#main
	 */
	public static void interpret() {
		// INIT
		registerGlobalVars();
		// RUNTIME
		print("\nStarting Program: " + UNDERLINE);
		execute(FuncManager.getLine("main"));
	}

	/**
	 * Executes a MainExpression.
	 *
	 * @param i      the line of the MainExpression.
	 * @param params are the passed parameters
	 * 
	 * @return false if this function shouldn't call any other functions afterwards.
	 *         {@link ReturnStatement#execute}
	 */
	public static boolean execute(int i) {
		MainExpression m = Main.PROGRAM.getLine(i).getMainExpression();
		print(m.getOriginalLine() + ": " + m.toString());
		return m.execute();
	}

	/**
	 * Registers and initialises all static Variables in the global scope.
	 * 
	 * (Outside of functions).
	 * 
	 * @see ProgramLine
	 * @see Declaration
	 */
	private static void registerGlobalVars() {
		for (ProgramLine l : Main.PROGRAM) {
			if (l.getMainExpression() instanceof Declaration d && d.getScope() == GlobalScope.GLOBAL) {
				print("Registering global Var " + d.getNameString() + " in line: " + d.getOriginalLine());
				d.getValue();
			}
		}
	}
}
