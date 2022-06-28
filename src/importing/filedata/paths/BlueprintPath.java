package importing.filedata.paths;

import building.expressions.main.blueprints.*;

public class BlueprintPath extends DataPath {
	
	public final Blueprint blueprint;
	
	public BlueprintPath(DataPath path, Blueprint blueprint) {
		super(path);
		this.blueprint = blueprint;
	}
	
	@Override
	public boolean equals(Object obj) {
		return super.equals(obj) && blueprint.equals(((BlueprintPath) obj).blueprint);
	}
	
	@Override
	public String toString() {
		return location + filepath + "(" + blueprint.getNameString() + ": " + orgLine + ")";
	}
}
