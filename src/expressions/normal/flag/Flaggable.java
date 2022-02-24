package expressions.normal.flag;

import java.util.Set;

import exceptions.parsing.UnexpectedFlagException;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.main.functions.Function;
import expressions.normal.containers.Variable;
import expressions.possible.assigning.Declaration;
import types.specific.FlagType;

/**
 * Anything that can have a {@link FlagType}.
 * 
 * @see Function
 * @see Variable
 * @see Declaration
 */
public interface Flaggable extends MergedExpression {

	/**
	 * Sets all Flags for this Expression.
	 * 
	 * @param flags is a list of all passed flags in front of this.
	 */
	void setFlags(Set<FlagType> flags) throws UnexpectedFlagException;

}