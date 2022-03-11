package building.expressions.abstractions.interfaces;

import java.util.Set;

import building.expressions.main.functions.Definition;
import building.expressions.main.statements.FlagSpace;
import building.expressions.normal.containers.Variable;
import building.expressions.possible.allocating.Declaration;
import building.types.specific.FlagType;

/**
 * Anything that can have a {@link FlagType}.
 * 
 * <pre>
 * Every subclass should contain:
 * (private) final Set<FlagType> flags = new HashSet<>();
 * </pre>
 * 
 * @see Definition
 * @see Variable
 * @see Declaration
 * @see FlagSpace
 */
public interface Flaggable {

	/**
	 * Add all Flags to this {@link Flaggable}. This should never get called in any constructor!
	 * 
	 * <pre>
	 *  Should look like this:
	 *  
	 * 	public final void setFlags(Set<FlagType> flags) {
	 * 		this.flags.addAll(flags); 
	 * 	}
	 * </pre>
	 * 
	 * @param flags is a list of all passed flags in front of this.
	 */
	void addFlags(Set<FlagType> flags);

	/**
	 * Returns true if this {@link Flaggable} contains the specified {@link FlagType}.
	 * 
	 * <pre>
	 *  Should look like this:
	 *  
	 *  public boolean hasFlag(FlagType f) {
	 *  	return flags.contains(f);
	 *  }
	 * </pre>
	 */
	boolean hasFlag(FlagType f);
}