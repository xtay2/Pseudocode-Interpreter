package runtime.defmanager;

import java.util.HashMap;
import java.util.List;

import building.expressions.main.functions.Definition;
import building.expressions.normal.containers.Name;
import building.expressions.possible.Call;
import runtime.exceptions.DeclarationException;
import runtime.exceptions.DefNotFoundException;

/** Registers and finds all {@link Definition}s. */
public abstract class DefManager {

	private static HashMap<String, Definition> memory = new HashMap<>();

	/** Register a {@link Definition} by its {@link Name}. */
	public static void register(Definition def) {
		if (memory.putIfAbsent(def.getNameString() + "<" + def.expectedParams() + ">", def) != null)
			throw new DeclarationException(def.getOriginalLine(), "Tried to register two definitions with the name \"" + def.getNameString()
					+ "\" and " + (def.expectedParams() == 1 ? "one parameter." : def.expectedParams() + "params."));
	}

	/**
	 * Find a {@link Definition}.
	 * 
	 * @param defName is the name of the {@link Definition}.
	 * @param params  is the number of parameters.
	 * @param orgLine is the original line of the {@link Call}. (Error-Handling)
	 * 
	 * @throws DefNotFoundException if the {@link Definition} wasn't found.
	 */
	public static Definition get(String defName, int params, int orgLine) throws DefNotFoundException {
		Definition def = memory.get(defName + "<" + params + ">");
		if (def == null) {
			List<String> similarNames = memory.keySet().stream().filter(e -> e.contains(defName)).toList();
			if (!similarNames.isEmpty()) {
				throw new DefNotFoundException(orgLine, "There is no definition called \"" + defName + "\" that takes " + params
						+ " parameter" + (params == 1 ? "" : "s") + ".\nSimilar defs in memory: " + similarNames);
			}
			throw new DefNotFoundException(orgLine, "There is no definition called " + defName + ".\nDefs in memory:"
					+ memory.keySet().stream().reduce("", (acc, e) -> acc + "\n-" + e));
		}
		return def;
	}

}
