package expressions.special;

import datatypes.Value;
import expressions.main.functions.Function;
import expressions.normal.Name;
import expressions.normal.array.ArrayAccess;
import expressions.normal.brackets.BracketedExpression;
import expressions.normal.operators.Operation;
import expressions.possible.Assignment;
import expressions.possible.Call;
import expressions.possible.Crement;

/**
 * An interface for everything that can return a Value in code.
 * 
 * <pre>
 * Raw ValueHolders
 * {@link Name} 
 * {@link Call} 
 * {@link Value} 
 * {@link ArrayAccess} 
 * {@link Crement}
 * 
 * Boxed ValueHolders
 * {@link Operation}
 * {@link BracketedExpression}
 * {@link Assignment}
 * {@link Function} 
 * </pre>
 */
public interface ValueHolder {

	public Value getValue();
}
