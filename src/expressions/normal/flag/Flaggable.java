package expressions.normal.flag;

import java.util.Set;

import exceptions.parsing.UnexpectedFlagException;
import expressions.main.functions.Returnable;
import expressions.normal.containers.Variable;
import expressions.possible.assigning.Declaration;
import types.specific.FlagType;

/**
 * Anything that can have a {@link FlagType}.
 * 
 * @see Returnable
 * @see Variable
 * @see Declaration
 */
public interface Flaggable {

	/**
	 * Sets all Flags for this Expression.
	 * 
	 * <pre>
	 *  Should look like this:
	 *  
	 * 	public final void setFlags(Set<FlagType> flags) throws UnexpectedFlagException {
	 * 		flags.addAll(flags); 
	 * 	}
	 * </pre>
	 * 
	 * @param flags is a list of all passed flags in front of this.
	 */
	void setFlags(Set<FlagType> flags) throws UnexpectedFlagException;

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