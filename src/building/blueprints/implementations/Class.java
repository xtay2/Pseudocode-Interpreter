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
 * -attributes
 * -methods
 * </pre>
 */
public class Class extends Blueprint {

	@Override
	protected BlueprintType[] inherits() {
		return new BlueprintType[] { BlueprintType.CLASS, BlueprintType.INTERFACE, BlueprintType.MODULE, BlueprintType.STRUCT };
	}

}