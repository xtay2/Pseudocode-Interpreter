package building.expressions.main.blueprints;

import building.expressions.normal.brackets.*;
import building.expressions.normal.containers.name.*;
import building.types.specific.*;

public class Module extends Blueprint {
	
	public Module(int lineID, Name name, OpenBlock ob) {
		super(lineID, BlueprintType.MODULE, name, ob);
	}
}
