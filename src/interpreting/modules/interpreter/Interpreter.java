package interpreting.modules.interpreter;

import static misc.helper.Output.UNDERLINE;
import static misc.helper.Output.print;

import building.expressions.abstractions.GlobalScope;
import building.expressions.abstractions.MainExpression;
import building.expressions.main.functions.MainFunction;
import building.expressions.main.functions.Definition;
import building.expressions.main.statements.ReturnStatement;
import building.expressions.normal.containers.Variable;
import building.expressions.possible.allocating.Declaration;
import interpreting.program.ProgramLine;
import misc.main.Main;

public final class Interpreter {

	/**
	 * Registeres every {@link Variable} and {@link Definition} and starts the interpreting-process by
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
		// Call Main
		GlobalScope.GLOBAL.getMain().call();
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
