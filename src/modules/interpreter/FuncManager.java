package modules.interpreter;

import java.util.HashMap;

import exceptions.runtime.DeclarationException;
import exceptions.runtime.IllegalCallException;
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

	public static void registerFunction(String name, int line) {
		if (funcPositions.containsKey(name))
			throw new DeclarationException(Main.PROGRAM.getLine(line).lineIndex, "Duplicate function declaration. func " + name);
		funcPositions.put(name, line);
	}
}
