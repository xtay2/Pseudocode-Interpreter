package modules.interpreter;

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
import expressions.main.functions.MainFunction;
import expressions.main.functions.Returnable;
import expressions.main.statements.ReturnStatement;
import main.Main;
import modules.parser.program.Program;
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
		Returnable f = (Returnable) Main.PROGRAM.getLine(FuncManager.getLine(name + params.length)).getMainExpression();
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
	 * Registeres all variables and functions and starts the interpreting-process by calling the main
	 * function
	 * 
	 * @param program is the program that gets interpreted.
	 * 
	 * @see Main#main
	 */
	public static void interpret(Program program) {
		// INIT
		registerFunctions();
		registerGlobalVars();
		// RUNTIME
		print("\nStarting Program: " + UNDERLINE);
		execute(FuncManager.getLine("main"));
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
			currentScope = Main.PROGRAM.getLine(i).searchForScope();
			print(e.toString() + " in " + currentScope.getScopeName());
			// Check func in other func
			if (e instanceof Returnable r && currentScope != GlobalScope.GLOBAL)
				throw new IllegalCodeFormatException(Main.PROGRAM.getLine(i).lineIndex,
						"A function cannot be defined in another function. \nSee: \"" + r.getName() + "\" in "
								+ currentScope.getScopeName());
			// Check doppelte Main
			if (e instanceof MainFunction) {
				if (hasMain)
					throw new DeclarationException(Main.PROGRAM.getLine(i).lineIndex, "The main-function should be defined only once!");
				FuncManager.registerFunction(KeywordType.MAIN.toString(), i);
				hasMain = true;
			}
			// Speichere alle Funktionsnamen (Main darf nicht gecallt werden.)
			if (e instanceof Returnable r)
				FuncManager.registerFunction(r.getName() + r.expectedParams(), i);
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
			if (Main.PROGRAM.getLine(i).getMainExpression() instanceof Declaration d
					&& Main.PROGRAM.getLine(i).searchForScope() == GlobalScope.GLOBAL)
				d.initAndRegister();
		}
	}
}
