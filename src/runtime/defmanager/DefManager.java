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
import errorhandeling.PseudocodeException;
import importing.filedata.paths.DataPath;

/** Registers and finds all {@link Definition}s. */
public abstract class DefManager {

	private static Set<Definition> memory = new HashSet<>(), finalizedFuncs = new HashSet<>();

	/** Register a {@link Definition} by its {@link Name}. */
	public static void register(Definition def) {
		assert memory.add(def) : "Multiple definitions of " + def + ". This should get checked by the importer.";
	}

	/**
	 * Find a {@link Definition}.
	 *
	 * @param defName is the name of the {@link Definition}.
	 * @param params is the number of parameters.
	 * @param dataPath is the {@link DataPath} of the {@link Call}. (Error-Handling)
	 *
	 * @throws DefNotFoundException if the {@link Definition} wasn't found.
	 */
	public static Definition get(String defName, int params, DataPath dataPath) {
		Definition def = find(memory, e -> e.getNameString().equals(defName) && e.expectedParams() == params);
		if (def == null) {
			// Finalized-Check
			if ((def = find(finalizedFuncs, e -> e.getNameString().equals(defName) && e.expectedParams() == params)) != null) {
				throw new PseudocodeException("CalledFinalDef", //
						"The final function \"" + def + "\" was already called.", //
						def.getDataPath());
			}

			// Contains-Check
			List<Definition> similarNames = new ArrayList<>();
			similarNames.addAll(memory);
			similarNames.addAll(finalizedFuncs);
			similarNames = similarNames.stream().filter(e -> e.getNameString().equals(defName)).toList();
			if (!similarNames.isEmpty()) {
				throw new PseudocodeException("DefNotFound",
						"There is no definition called \"" + defName + "\" that takes " + params + " parameter" + (params == 1 ? "" : "s")
								+ ".\nSimilar defs in memory: " + similarNames, //
						dataPath);
			}
			throw new PseudocodeException("DefNotFound", "There is no definition called \"" + defName + "\"." //
					+ "\nDefs in memory: " + enumerate(memory), dataPath);
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
	 */
	public static void finalize(Definition def) {
		if (!memory.remove(def))
			throw new AssertionError(def + " couldn't get finalized, as it wasn't in memory.");
		finalizedFuncs.add(def);
	}
}
