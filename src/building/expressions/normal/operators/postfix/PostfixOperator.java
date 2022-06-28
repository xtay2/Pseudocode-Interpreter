package building.expressions.normal.operators.postfix;

import static runtime.datatypes.numerical.NumberValue.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.normal.operators.infix.*;
import building.expressions.normal.operators.prefix.*;
import building.expressions.possible.multicall.*;
import building.types.specific.operators.*;
import errorhandeling.*;
import runtime.datatypes.*;
import runtime.datatypes.array.*;

/**
 * @see PrefixOperator
 * @see InfixOperator
 */
public class PostfixOperator extends PossibleMainExpression implements MultiCallableValueHolder {
	
	private final ValueHolder content;
	
	public PostfixOperator(int lineID, PostfixOpType type, ValueHolder content) {
		super(lineID, type);
		this.content = content;
		if (content == null)
			throw new AssertionError("Val cannot be null.");
	}
	
	@Override
	public Value getValue() {
		if (content instanceof MultiCall mc)
			return executeFor(mc.content);
		return evaluate(content);
	}
	
	@Override
	public Value executeFor(ValueHolder[] content) {
		Value[] res = new Value[content.length];
		for (int i = 0; i < content.length; i++)
			res[i] = evaluate(content[i]);
		return ArrayValue.newInstance(res);
	}
	
	private Value evaluate(ValueHolder val) {
		Value v = val.getValue();
		try {
			switch ((PostfixOpType) type) {
				case INC:
					if (val instanceof ValueChanger incVar)
						incVar.setValue(v.asNr().add(ONE));
					else {
						throw new PseudocodeException("InvalidPostfix", //
								"You cannot post-increment a " + ((Expression) val).type + ".", //
								getBlueprintPath());
					}
					break;
				case DEC:
					if (val instanceof ValueChanger decVar)
						decVar.setValue(v.asNr().sub(ONE));
					else {
						throw new PseudocodeException("InvalidPostfix", //
								"You cannot post-decrement a " + ((Expression) val).type + ".", //
								getBlueprintPath());
					}
					break;
				case FAC:
					v = v.asInt().fac();
					break;
			}
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, getBlueprintPath());
		}
		return v;
	}
	
	@Override
	public boolean execute() {
		getValue();
		return callNextLine();
	}
}
