package runtime.defmanager;

import static misc.helper.CollectionHelper.find;
import static misc.helper.StringHelper.enumerate;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.functions.Definition;
import building.expressions.normal.containers.Name;
import building.expressions.possible.Call;
import runtime.exceptions.DeclarationException;
import runtime.exceptions.DefNotFoundException;
import runtime.exceptions.IllegalCallException;

/** Registers and finds all {@link Definition}s. */
public abstract class DefManager {

	private static Set<Definition> memory = new HashSet<>();
	private static Set<Definition> finalizedFuncs = new HashSet<>();

	/** Register a {@link Definition} by its {@link Name}. */
	public static void register(Definition def) {
		if (!memory.add(def))
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
		Definition def = find(memory, e -> e.getNameString().equals(defName) && e.expectedParams() == params);
		if (def == null) {
			// Finalized-Check
			if ((def = find(finalizedFuncs, e -> e.getNameString().equals(defName) && e.expectedParams() == params)) != null)
				throw new IllegalCallException(orgLine, "The final function " + def + " was already called.");

			// Contains-Check
			List<Definition> similarNames = new ArrayList<>();
			similarNames.addAll(memory);
			similarNames.addAll(finalizedFuncs);
			similarNames = similarNames.stream().filter(e -> e.getNameString().equals(defName)).toList();
			if (!similarNames.isEmpty()) {
				throw new DefNotFoundException(orgLine, "There is no definition called \"" + defName + "\" that takes " + params
						+ " parameter" + (params == 1 ? "" : "s") + ".\nSimilar defs in memory: " + similarNames);
			}
			throw new DefNotFoundException(orgLine,
					"There is no definition called \"" + defName + "\".\nDefs in memory:" + enumerate(memory));
		}
		return def;
	}

	/**
	 * If a {@link Definition} thats declared as final gets called, it hereby gets erased from the
	 * active memory, to reduce redundancy.
	 * 
	 * Gets called in {@link Definition#call(ValueHolder...)}
	 * 
	 * @param def the definition should pass itself.
	 * 
	 */
	public static void finalize(Definition def) {
		if (!memory.remove(def))
			throw new AssertionError(def + " couldn't get finalized, as it wasn't in memory.");
		finalizedFuncs.add(def);
	}
}
