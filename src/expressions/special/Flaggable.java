package expressions.special;

import java.util.List;

import exceptions.parsing.UnexpectedFlagException;
import expressions.main.functions.Function;
import expressions.normal.Flag;
import expressions.normal.Variable;

/**
 * Anything that can have a {@link Flag} like native.
 * 
 * @see Function
 * 
 * (In future)
 * @see Variable
 */
public interface Flaggable extends MergedExpression {
	
	/**
	 * Sets all Flags for this Expression.
	 * @param flags is a list of all passed flags in front of this.
	 */
	void setFlags(List<Flag> flags) throws UnexpectedFlagException;	
	
	boolean isNative();
	
}