package building.types.specific;

import java.lang.Module;

import building.expressions.main.blueprints.*;
import building.types.abstractions.*;

/**
 * This is used to identify the different {@link Blueprint}s.
 */
public enum BlueprintType implements SpecificType {
	
	/** {@link Class} */
	// CLASS,
	
	/** {@link Enum} */
	// ENUM,
	
	/** {@link Interface} */
	// INTERFACE,
	
	/** {@link Struct} */
	// STRUCT,
	
	/** {@link Module} */
	MODULE;
	
	@Override
	public AbstractType[] abstractExpected() {
		return new AbstractType[] {DynamicType.NAME};
	}
	
	@Override
	public String toString() {
		return name().toLowerCase();
	}
}