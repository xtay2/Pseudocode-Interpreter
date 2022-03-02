package modules.interpreter;

import java.util.HashMap;

import exceptions.runtime.DeclarationException;
import exceptions.runtime.IllegalCallException;
import expressions.main.functions.Function;
import expressions.main.functions.Returnable;
import main.Main;

public abstract class FuncManager {

	/** Save the line indices of all functions */
	private static final HashMap<String, Integer> funcPositions = new HashMap<>();

	/**
	 * Returns the line of the function with the given name.
	 */
	public static int getLine(String name) {
		Integer line = funcPositions.get(name);
		if (line == null) {
			throw new IllegalCallException(-1, "\nThe called function " + name + " didn't get registered." + "\nExisting functions: \n"
					+ funcPositions.keySet().stream().reduce("", (err, k) -> err + " " + funcPositions.get(k) + ": " + k + "\n"));
		}
		return line;
	}

	/**
	 * Registers a {@link Function} with an unique name.
	 * 
	 * Gets called in {@link Interpreter#registerFunctions()}.
	 */
	public static void registerFunction(String name, int line) {
		if (funcPositions.containsKey(name))
			throw new DeclarationException(Main.PROGRAM.getLine(line).orgLine, "Duplicate function declaration. func " + name);
		if (line < 0)
			throw new AssertionError("LineID cannot be negative, was: " + line + " for function " + name + ".");
		funcPositions.put(name, line);
	}

	/**
	 * Returns the quested {@link Returnable}, or throws an {@link IllegalCallException} if it wasn't
	 * declared.
	 */
	public static Returnable findFunc(String name) {
		return (Returnable) Main.PROGRAM.getLine(getLine(name)).getMainExpression();
	}
}
