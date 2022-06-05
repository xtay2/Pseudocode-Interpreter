package building.blueprints;

import building.blueprints.implementations.Class;
import building.blueprints.implementations.Enum;
import building.blueprints.implementations.Interface;
import building.blueprints.implementations.Module;
import building.blueprints.implementations.Struct;

/**
 * This is used to identify the different {@link Blueprint}s.
 */
public enum BlueprintType {

	/** {@link Class} */
	CLASS,

	/** {@link Enum} */
	ENUM,

	/** {@link Interface} */
	INTERFACE,

	/** {@link Module} */
	MODULE,

	/** {@link Struct} */
	STRUCT;
}