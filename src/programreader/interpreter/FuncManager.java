package programreader.interpreter;

import java.util.HashMap;

import exceptions.DeclarationException;

public final class FuncManager {
	
	private FuncManager() {
		
	}
	
	/** Save the line indices of all functions */
	private static HashMap<String, Integer> funcPositions = new HashMap<>();

	public static void registerFunction(String name, int line) {
		if (funcPositions.containsKey(name))
			throw new DeclarationException("Duplicate function declaration. func " + name);
		funcPositions.put(name, line);
	}

	/**
	 * Returns the line of the function with the given name.
	 */
	public static int getLine(String name) {
		Integer line = funcPositions.get(name);
		if (line == null)
			throw new IllegalArgumentException("The called function " + name + " doesn't exist." + "\n Existing functions: " + funcPositions);
		return line;
	}
}
