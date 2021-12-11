package interpreter;

import static helper.Output.LINE_BREAK;
import static helper.Output.UNDERLINE;
import static helper.Output.print;

import datatypes.Castable;
import exceptions.DeclarationException;
import expressions.main.Declaration;
import expressions.main.functions.Function;
import expressions.main.functions.MainFunction;
import expressions.special.MainExpression;
import expressions.special.Scope;
import expressions.special.Value;
import expressions.special.ValueHolder;
import interpreter.system.SystemFunctions;
import main.Main;
import parser.program.KeywordType;
import parser.program.Program;

public final class Interpreter {

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

	private static void registerFunctions() {
		print("Pre-Compiling:" + UNDERLINE);
		boolean hasMain = false;
		for (int i = 0; i < Main.program.size(); i++) {
			MainExpression e = Main.program.getLine(i).getExpression();
			print(e.toString());
			if (e instanceof MainFunction) {
				if (hasMain)
					throw new DeclarationException("The main-function should be defined only once!");
				FuncManager.registerFunction(KeywordType.MAIN.keyword, i);
				hasMain = true;
			}
			// Speichere alle Funktionsnamen (Main darf nicht gecallt werden.)
			if (e instanceof Function && !(e instanceof MainFunction))
				FuncManager.registerFunction(((Function) e).getName() + ((Function) e).expectedParams(), i);
		}
		if (!hasMain)
			throw new DeclarationException("Program has to include a main-function!");
	}

	private static void registerGlobalVars() {
		for (int i = 0; i < Main.program.size(); i++) {
			MainExpression e = Main.program.getLine(i).getExpression();
			if (e instanceof Declaration && ((Declaration) e).getName().getScope().equals(Scope.GLOBAL_SCOPE))
				execute(i, false);
		}
	}

	/**
	 * Executes a MainExpression.
	 *
	 * @param name          the line of the MainExpression.
	 * @param doExecuteNext if the called function should execute the one in the
	 *                      following line. Standart: true
	 * @param params        are the passed parameters
	 * 
	 * @return false if this function shouldn't call any other functions. @see
	 *         ReturnStatement execute
	 */
	public static boolean execute(int i, boolean doExecuteNext, ValueHolder... params) {
		return Main.program.getLine(i).getExpression().execute(doExecuteNext, params);
	}

	/**
	 * Executes a MainExpression.
	 *
	 * @param name          the name of the MainExpression.
	 * @param doExecuteNext if the called function should execute the one in the
	 *                      following line. Standart: true
	 * @param params        are the passed parameters
	 * 
	 * @return false if this function shouldn't call any other functions. @see
	 *         ReturnStatement execute
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
	public static Castable call(String name, boolean doExecuteNext, ValueHolder... params) {
		SystemFunctions.SYSTEM_FUNCTION sysFunc = SystemFunctions.isSystemFunction(name);
		if (sysFunc != null)
			return SystemFunctions.callSystemFunc(sysFunc, params);
		Function f = (Function) Main.program.getLine(FuncManager.getLine(name + params.length)).getExpression();
		f.execute(doExecuteNext, params);
		Castable returnVal = f.getValue();
		f.setReturnVal(null);
		return returnVal;
	}
}
