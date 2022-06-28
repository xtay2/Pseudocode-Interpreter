package building.expressions.abstractions.interfaces;

import building.expressions.main.statements.*;
import building.expressions.normal.brackets.*;
import building.expressions.normal.containers.*;
import building.expressions.normal.containers.name.*;
import building.expressions.normal.operators.*;
import building.expressions.possible.*;
import building.expressions.possible.allocating.*;
import building.expressions.possible.multicall.*;
import building.types.specific.datatypes.*;
import errorhandeling.*;
import runtime.datatypes.*;

/**
 * An interface for everything that can return a {@link Value} in code.
 *
 * <pre>
 * Raw ValueHolders
 * {@link Name}
 * {@link Call}
 * {@link Value}
 * {@link ArrayAccess}
 *
 * Boxed ValueHolders
 * {@link IsStatement}
 * {@link Operation}
 * {@link BracketedExpression}
 * {@link Assignment}
 * {@link MultiCall}
 * </pre>
 */
public interface ValueHolder extends Operatable, Castable {
	
	public Value getValue();
	
	/**
	 * This should only get overridden by {@link Value} and {@link MaybeValue}!
	 *
	 * @param t is the target type.
	 * @throws NonExpressionException -> Casting
	 */
	@Override
	default Value as(DataType t) throws NonExpressionException {
		return getValue().as(t);
	}
	
	/**
	 * Throws a "CastingException".
	 *
	 * @return Nothing, but has this return type, so that the method can be used in switch-expressions.
	 */
	static Value throwCastingExc(Value val, DataType targetType) throws NonExpressionException {
		throw new NonExpressionException("Casting", "Cannot cast " + val + " to " + targetType + ".");
	}
}
