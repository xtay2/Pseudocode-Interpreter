package building.blueprints;

import building.blueprints.implementations.Class;
import building.blueprints.implementations.Enum;
import building.blueprints.implementations.Interface;
import building.blueprints.implementations.Module;
import building.blueprints.implementations.Struct;
import building.expressions.abstractions.ScopeHolder;

/**
 * A {@link Blueprint} is a {@link ScopeHolder} with different properties.
 * 
 * <pre>
 * Generally, a {@link Blueprint} holds the info for all:
 * -constants
 * -functions
 * -(variable) attributes
 * -methods
 * -constructors
 * </pre>
 * 
 * @see Struct
 * @see Module
 * @see Enum
 * @see Class
 * @see Interface
 */
public abstract class Blueprint {

	/** Returns an array of all {@link BlueprintType}s that this one can extend. */
	protected abstract BlueprintType[] inherits();

}
