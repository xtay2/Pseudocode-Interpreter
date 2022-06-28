package building.expressions.normal.operators.infix;

import building.expressions.abstractions.interfaces.*;
import building.types.specific.operators.*;
import errorhandeling.*;
import misc.constants.*;
import runtime.datatypes.*;
import runtime.datatypes.array.*;

public class InOperator extends InfixOperator {
	
	public InOperator(int lineID, InfixOpType op) {
		super(lineID, op);
	}
	
	@Override
	public BoolValue perform(ValueHolder a, ValueHolder b) {
		try {
			return ((ArrayValue) b.as(TypeConstants.VAR_ARR)).contains(a.getValue());
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, getBlueprintPath());
		}
	}
}
