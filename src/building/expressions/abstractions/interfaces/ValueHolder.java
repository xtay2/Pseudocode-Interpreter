package building.expressions.abstractions.interfaces;

import building.expressions.main.statements.IsStatement;
import building.expressions.normal.brackets.BracketedExpression;
import building.expressions.normal.containers.ArrayAccess;
import building.expressions.normal.containers.Name;
import building.expressions.normal.operators.Operation;
import building.expressions.possible.Call;
import building.expressions.possible.allocating.Assignment;
import building.expressions.possible.multicall.MultiCall;
import building.types.specific.datatypes.DataType;
import runtime.datatypes.Value;
import runtime.exceptions.CastingException;

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
	 * This should not get overridden!
	 */
	@Override
	default Value as(DataType t) throws CastingException {
		return getValue().as(t);
	}
}
