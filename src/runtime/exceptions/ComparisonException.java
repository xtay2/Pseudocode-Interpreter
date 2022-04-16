package runtime.exceptions;

import building.types.specific.operators.InfixOpType;
import runtime.datatypes.Value;

/**
 * Gets thrown, when two uncomparable types are compared with one of:
 * 
 * <pre>
 * {@link InfixOpType#EQUALS}
 * {@link InfixOpType#NOT_EQUALS}
 * 
 * {@link InfixOpType#GREATER} 
 * {@link InfixOpType#GREATER_EQ}
 * 
 * {@link InfixOpType#LESS} 
 * {@link InfixOpType#LESS_EQ}
 * </pre>
 *
 */
@SuppressWarnings("serial")
public class ComparisonException extends AbstractRuntimeException {

	public ComparisonException(Value a, Value b) {
		super(-1, "Tried to compare " + a + " to " + b + ".");
	}

}
