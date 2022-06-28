package building.expressions.normal.operators.prefix;

import static runtime.datatypes.numerical.NumberValue.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import building.expressions.normal.operators.infix.*;
import building.expressions.normal.operators.postfix.*;
import building.expressions.possible.multicall.*;
import building.types.specific.operators.*;
import errorhandeling.*;
import runtime.datatypes.*;
import runtime.datatypes.array.*;

/**
 * @see PostfixOperator
 * @see InfixOperator
 */
public class PrefixOperator extends PossibleMainExpression implements MultiCallableValueHolder {
	
	private final ValueHolder content;
	
	public PrefixOperator(int lineID, PrefixOpType type, ValueHolder content) {
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
			switch ((PrefixOpType) type) {
				case INC:
					v = v.asNr().add(ONE);
					if (val instanceof ValueChanger vc)
						vc.setValue(v);
					break;
				case DEC:
					v = v.asNr().sub(ONE);
					if (val instanceof ValueChanger vc)
						vc.setValue(v);
					break;
				case NEG:
					v = v.asNr().negate();
					break;
				case NOT:
					v = v.asBool().not();
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
