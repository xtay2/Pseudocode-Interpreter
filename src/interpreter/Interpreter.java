package interpreter;

import static helper.Output.LINE_BREAK;
import static helper.Output.UNDERLINE;
import static helper.Output.print;

import datatypes.Value;
import exceptions.parsing.IllegalCodeFormatException;
import exceptions.runtime.DeclarationException;
import expressions.main.Declaration;
import expressions.main.functions.Function;
import expressions.main.functions.MainFunction;
import expressions.special.MainExpression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import interpreter.system.SystemFunctions;
import main.Main;
import parser.program.KeywordType;
import parser.program.Program;

public final class Interpreter {

	/**
	 * Registeres all variables and functions and starts the interpreting-process by
	 * calling the main function
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
		execute(FuncManager.getLine("main"), true);
		// Cleanup
		print("Program has ended.");
		print(LINE_BREAK);
	}

	/**
	 * Register all functions in the program, so that they are accessable through
	 * the call-Method.
	 * 
	 * @see Interpreter#call
	 */
	private static void registerFunctions() {
		print("Pre-Compiling:" + UNDERLINE);
		boolean hasMain = false;
		Scope currentScope = Scope.GLOBAL_SCOPE;
		for (int i = 0; i < Main.program.size(); i++) {
			MainExpression e = Main.program.getLine(i).getMainExpression();
			print(e.toString());
			//Check func in other func
			if(e instanceof Function && currentScope != Scope.GLOBAL_SCOPE)
				throw new IllegalCodeFormatException("A function cannot be defined in another function.");
			//Check doppelte Main
			if (e instanceof MainFunction) {
				if (hasMain)
					throw new DeclarationException("The main-function should be defined only once!");
				FuncManager.registerFunction(KeywordType.MAIN.keyword, i);
				hasMain = true;
			}
			// Speichere alle Funktionsnamen (Main darf nicht gecallt werden.)
			if (e instanceof Function && !(e instanceof MainFunction))
				FuncManager.registerFunction(((Function) e).getName() + ((Function) e).expectedParams(), i);
			currentScope = Main.program.getLine(i).searchForScope();
		}
		if (!hasMain)
			throw new DeclarationException("Program has to include a main-function!");
	}

	/**
	 * Register all Variables in the global scope. (Outside of functions).
	 */
	private static void registerGlobalVars() {
		for (int i = 0; i < Main.program.size(); i++) {
			MainExpression e = Main.program.getLine(i).getMainExpression();
			if (e instanceof Declaration && ((Declaration) e).getName().getScope().equals(Scope.GLOBAL_SCOPE))
				execute(i, false);
		}
	}

	/**
	 * 
	 * Executes a MainExpression.
	 *
	 * @param name          the line of the MainExpression.
	 * @param doExecuteNext if the called function should execute the one in the
	 *                      following line. Standart: true
	 * @param params        are the passed parameters
	 * 
	 * @return false if this function shouldn't call any other functions.
	 *         ReturnStatement#execute
	 * 
	 */
	public static boolean execute(int i, boolean doExecuteNext, ValueHolder... params) {
		return Main.program.getLine(i).getMainExpression().execute(doExecuteNext, params);
	}

	/**
	 * Executes a MainExpression.
	 *
	 * @param name          the name of the MainExpression.
	 * @param doExecuteNext if the called function should execute the one in the
	 *                      following line. Standart: true
	 * @param params        are the passed parameters
	 * 
	 * @return false if this function shouldn't call any other functions.
	 *         ReturnStatement#execute
	 */
	public static boolean execute(String name, boolean doExecuteNext, ValueHolder... params) {
		return execute(FuncManager.getLine(name + params.length), doExecuteNext);
	}

	/**
	 * Calls a function and returns it return-value.
	 *
	 * @param name          is the name of the function.
	 * @param doExecuteNext if the called function should execute the one in the
	 *                      following line. Standart: true
	 * @param params        are the function-parameters
	 * @return the return-value of the function.
	 */
	public static Value call(String name, boolean doExecuteNext, ValueHolder... params) {
		SystemFunctions.SYSTEM_FUNCTION sysFunc = SystemFunctions.isSystemFunction(name);
		if (sysFunc != null)
			return SystemFunctions.callSystemFunc(sysFunc, params);
		Function f = (Function) Main.program.getLine(FuncManager.getLine(name + params.length)).getMainExpression();
		f.execute(doExecuteNext, params);
		Value returnVal = f.getValue();
		f.setReturnVal(null);
		return returnVal;
	}
}
