package expressions.normal.flag;

import java.util.List;

import exceptions.parsing.UnexpectedFlagException;
import expressions.abstractions.MergedExpression;
import expressions.main.functions.Function;
import expressions.normal.containers.Variable;
import expressions.special.BuilderExpression;
import parsing.program.KeywordType;

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
	void setFlags(List<KeywordType> flags) throws UnexpectedFlagException;	
	
	boolean isNative();
	
}