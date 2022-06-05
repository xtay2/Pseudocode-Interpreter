package building.blueprints.implementations;

import building.blueprints.Blueprint;
import building.blueprints.BlueprintType;

/**
 * An {@link Interface} is a {@link Blueprint} with the following properties:
 * 
 * <pre>
 * It stores:
 * -constants (public/private)
 * -functions
 * -(abstract) attributes
 * -(abstract) methods
 * </pre>
 */
public class Interface extends Blueprint {

	@Override
	protected BlueprintType[] inherits() {
		return new BlueprintType[] { BlueprintType.INTERFACE, BlueprintType.CLASS, BlueprintType.MODULE, BlueprintType.STRUCT };
	}
}
