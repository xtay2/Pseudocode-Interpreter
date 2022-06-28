package building.expressions.main.blueprints;

import java.util.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.normal.brackets.*;
import building.expressions.normal.containers.name.*;
import building.types.abstractions.*;
import building.types.specific.*;
import importing.filedata.paths.*;
import launching.*;

/**
 * @see Module
 */
public abstract class Blueprint extends BlockHolder implements Flaggable, NameHolder {
	
	private final Name name;
	
	public Blueprint(int lineID, SpecificType myType, Name name, OpenBlock ob) {
		super(lineID, myType, ob);
		this.name = name;
	}
	
	@Override
	public Name getName() { return name; }
	
	@Override
	public BlueprintPath getBlueprintPath() { return new BlueprintPath(Main.PROGRAM.getLine(lineIdentifier).getDataPath(), this); }
	
	@Override
	public boolean execute() {
		throw new AssertionError("A " + getClass().getSimpleName() + " doesn't support execution.");
	}
	
	private final Set<FlagType> flags = new HashSet<>();
	
	@Override
	public final void addFlags(Set<FlagType> flags) {
		this.flags.addAll(flags);
	}
	
	@Override
	public final boolean hasFlag(FlagType f) {
		return flags.contains(f);
	}
	
	@Override
	public String toString() {
		return getClass().getSimpleName() + ":" + name;
	}
}
