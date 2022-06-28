package runtime.defmanager;

import building.expressions.abstractions.interfaces.*;
import building.expressions.abstractions.scopes.*;
import building.expressions.main.blueprints.*;
import building.expressions.main.functions.*;
import building.expressions.normal.containers.name.*;
import building.expressions.possible.*;
import importing.filedata.paths.*;
import misc.util.*;

/** Registers and finds all {@link Definition}s. */
@Deprecated
public abstract class DefManager {
	
	/** Register a {@link Definition} by its {@link Name}. */
	public static void register(Definition def) {
		ScopeManager.DEFS.register(def.getBlueprintPath().blueprint, def);
	}
	
	/**
	 * Find a {@link Definition}.
	 *
	 * @param defName is the name of the {@link Definition}.
	 * @param params is the number of parameters.
	 * @param blueprintPath is the {@link DataPath} of the {@link Call}. (Error-Handling)
	 *
	 * @throws DefNotFoundException if the {@link Definition} wasn't found.
	 */
	public static Definition get(String defName, int params, Blueprint targetBlueprint) {
		return ScopeManager.DEFS.find(targetBlueprint, new ID<>(defName, params));
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
		// TODO
	}
}
