package building.expressions.main.statements;

import static building.types.SuperType.MERGED;

import java.util.HashSet;
import java.util.Set;

import building.expressions.abstractions.BlockHolder;
import building.expressions.abstractions.interfaces.Flaggable;
import building.expressions.normal.brackets.OpenBlock;
import building.types.specific.FlagType;

public class FlagSpace extends BlockHolder implements Flaggable {

	/** Flags for this {@link FlagSpace}. */
	private final Set<FlagType> flags = new HashSet<>();

	/**
	 * Constructs a {@link FlagSpace} out of a name and its scope.
	 * 
	 * @param name shouldn't be null
	 * @param os   shouldn't be null
	 */
	public FlagSpace(int lineID, OpenBlock ob) {
		super(lineID, MERGED, ob);
	}

	@Override
	public final void addFlags(Set<FlagType> flags) {
		this.flags.addAll(flags);
	}

	/** Returns all flags of this {@link FlagSpace}. */
	public final Set<FlagType> getFlags() {
		return flags;
	}

	@Override
	public final boolean hasFlag(FlagType f) {
		return flags.contains(f);
	}

	@Override
	public boolean execute() {
		callFirstLine();
		return callNextLine();
	}
}
