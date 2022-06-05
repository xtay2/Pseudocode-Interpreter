package building.blueprints.implementations;

import building.blueprints.Blueprint;
import building.blueprints.BlueprintType;

/**
 * A {@link Module} is a {@link Blueprint} with the following properties:
 * 
 * <pre>
 * It stores:
 * -constants (public/private)
 * -functions
 * -attributes
 * </pre>
 */
public class Module extends Blueprint {

	@Override
	protected BlueprintType[] inherits() {
		return new BlueprintType[] { BlueprintType.MODULE };
	}

}
