package building.blueprints.implementations;

import building.blueprints.Blueprint;
import building.blueprints.BlueprintType;

/**
 * A {@link Struct} is a {@link Blueprint} with the following properties:
 * 
 * <pre>
 * It stores:
 * -constants (public/private)
 * -attributes
 * </pre>
 */
public class Struct extends Blueprint {

	@Override
	protected BlueprintType[] inherits() {
		return new BlueprintType[] { BlueprintType.STRUCT };
	}

}
