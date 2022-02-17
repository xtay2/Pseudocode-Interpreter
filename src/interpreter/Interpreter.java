package interpreter;

import static helper.Output.LINE_BREAK;
import static helper.Output.UNDERLINE;
import static helper.Output.print;

import datatypes.Value;
import exceptions.parsing.IllegalCodeFormatException;
import exceptions.runtime.DeclarationException;
import expressions.abstractions.GlobalScope;
import expressions.abstractions.MainExpression;
import expressions.abstractions.Scope;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.Declaration;
import expressions.main.functions.Function;
import expressions.main.functions.MainFunction;
import expressions.main.statements.ReturnStatement;
import main.Main;
import parsing.program.Program;
import types.specific.KeywordType;

public final class Interpreter {

	/**
	 * Calls a function and returns it return-value.
	 *
	 * @param name          is the name of the function.
	 * @param doExecuteNext if the called function should execute the one in the following line.
	 *                      Standart: true
	 * @param params        are the function-parameters
	 * @return the return-value of the function.
	 */
	public static Value call(String name, ValueHolder... params) {
		Function f = (Function) Main.PROGRAM.getLine(FuncManager.getLine(name + params.length)).getMainExpression();
		f.execute(params);
		return f.retrieveReturnValue();
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
	public static boolean execute(int i, ValueHolder... params) {
		return Main.PROGRAM.getLine(i).getMainExpression().execute(params);
	}

	/**
	 * Executes a Function.
	 *
	 * @param name   the name of the MainExpression.
	 * @param params are the passed parameters
	 * 
	 * @return false if this function shouldn't call any other functions afterwards.
	 *         {@link ReturnStatement#execute}
	 */
	@Deprecated
	public static boolean execute(String name, ValueHolder... params) {
		return execute(FuncManager.getLine(name + params.length));
	}

	/**
	 * Registeres all variables and functions and starts the interpreting-process by calling the main
	 * function
	 * 
	 * @param program is the program that gets interpreted.
	 * 
	 * @see Main#main
	 */
	public static void interpret(Program program) {
		registerFunctions();
		print(LINE_BREAK);
		print("Initialising global Vars: " + UNDERLINE);
		registerGlobalVars();
		print("\nStarting Program: " + UNDERLINE);
		execute(FuncManager.getLine("main"));
		// Cleanup
		print("Program has ended.");
		print(LINE_BREAK);
	}

	/**
	 * Register all functions in the program, so that they are accessable through the call-Method.
	 * 
	 * @see Interpreter#call(String, ValueHolder...)
	 */
	private static void registerFunctions() {
		print("Pre-Compiling:" + UNDERLINE);
		boolean hasMain = false;
		Scope currentScope = GlobalScope.GLOBAL;
		for (int i = 0; i < Main.PROGRAM.size(); i++) {
			MainExpression e = Main.PROGRAM.getLine(i).getMainExpression();
			print(e.toString());
			// Check func in other func
			if (e instanceof Function f && currentScope != GlobalScope.GLOBAL)
				throw new IllegalCodeFormatException(Main.PROGRAM.getLine(i).lineIndex,
						"A function cannot be defined in another function. \nSee: \"" + f.getName() + "\" in " + currentScope);
			// Check doppelte Main
			if (e instanceof MainFunction) {
				if (hasMain)
					throw new DeclarationException(Main.PROGRAM.getLine(i).lineIndex, "The main-function should be defined only once!");
				FuncManager.registerFunction(KeywordType.MAIN.toString(), i);
				hasMain = true;
			}
			// Speichere alle Funktionsnamen (Main darf nicht gecallt werden.)
			if (e instanceof Function && !(e instanceof MainFunction))
				FuncManager.registerFunction(((Function) e).getName() + ((Function) e).expectedParams(), i);
			currentScope = Main.PROGRAM.getLine(i).searchForScope();
		}
		if (!hasMain)
			throw new AssertionError("Program has to include a main-function!");
	}

	/**
	 * Registers and initialises all static Variables in the global scope.
	 * 
	 * (Outside of functions).
	 */
	private static void registerGlobalVars() {
		for (int i = 0; i < Main.PROGRAM.size(); i++) {
			if (Main.PROGRAM.getLine(i).getMainExpression() instanceof Declaration d)
				d.registerIfGlobal();
		}
	}
}
