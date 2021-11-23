package programreader.interpreter;

import static helper.Output.*;

import exceptions.DeclarationException;
import programreader.expressions.main.Declaration;
import programreader.expressions.main.functions.Function;
import programreader.expressions.main.functions.MainFunction;
import programreader.expressions.special.MainExpression;
import programreader.expressions.special.Scope;
import programreader.expressions.special.Value;
import programreader.expressions.special.ValueHolder;
import programreader.program.KeywordType;
import programreader.program.Program;
import system.SystemFunctions;

public final class Interpreter {

	private static Program program;

	public static void interpret(Program program) {
		Interpreter.program = program;
		registerFunctions();
		print(LINE_BREAK);
		print("Initialising global Vars: " + UNDERLINE);
		registerGlobalVars();
		print("\nStarting Program: " + UNDERLINE);
		execute(FuncManager.getLine("main"), true);
		//Cleanup
		print("Program has ended.");
		print(LINE_BREAK);
	}

	private static void registerFunctions() {
		print("Pre-Compiling:" + UNDERLINE);
		boolean hasMain = false;
		for (int i = 0; i < program.size(); i++) {
			MainExpression e = program.getLine(i).getExpression();
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
		for (int i = 0; i < program.size(); i++) {
			MainExpression e = program.getLine(i).getExpression();
			if (e instanceof Declaration && ((Declaration) e).getName().getScope().equals(Scope.GLOBAL_SCOPE))
				execute(i, false);
		}
	}

	/**
	 * Executes a MainExpression.
	 * 
	 * @param name   the line of the MainExpression.
	 * @param params are the passed parameters
	 */
	public static boolean execute(int i, boolean doExecuteNext, ValueHolder... params) {
		return program.getLine(i).getExpression().execute(doExecuteNext, params);
	}

	/**
	 * Executes a MainExpression.
	 * 
	 * @param name   the name of the MainExpression.
	 * @param params are the passed parameters
	 */
	public static boolean execute(String name, boolean doExecuteNext, ValueHolder... params) {
		return execute(FuncManager.getLine(name + params.length), doExecuteNext);
	}

	/**
	 * Calls a function and returns it return-value.
	 * 
	 * @param name   is the name of the function.
	 * @param params are the function-parameters
	 * @return the return-value of the function.
	 */
	public static Value call(String name, boolean doExecuteNext, ValueHolder... params) {
		SystemFunctions.SYSTEM_FUNCTION sysFunc = SystemFunctions.isSystemFunction(name);
		if (sysFunc != null)
			return SystemFunctions.callSystemFunc(sysFunc, params);
		Function f = (Function) program.getLine(FuncManager.getLine(name + params.length)).getExpression();
		f.execute(doExecuteNext, params);
		Value returnVal = f.getValue();
		f.setReturnVal(null);
		return returnVal;
	}

	public static int getProgramLength() {
		return program.size();
	}
}
