package expressions.normal;

import exceptions.CastingException;
import expressions.special.Expression;
import expressions.special.Scope;
import expressions.special.Type;
import expressions.special.Value;
import interpreter.VarManager;
import parser.program.ExpressionType;

/**
 * Has a Name and a Value. The Name has a scope.
 *
 * @see TypedVar
 */
public class Variable extends Expression {

	private Name name = null;
	private Value value = null;

	public Variable(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.NAME);
	}

	/** Gets called when declared through Declaration or params in Function. */
	public void initialise(Name name, Value val) {
		this.name = name;
		setValue(val);
		VarManager.registerVar(this);
	}

	/** Should only get called by VarManager */
	public void setValue(Value val) throws CastingException {
		if (val == null)
			throw new NullPointerException("Value cannot be null.");
		if (getType() == null || val.getType() == getType())
			value = val;
		else
			value = switch (getType()) {
			case TEXT -> new Value(val.asText(), Type.TEXT);
			case BOOL -> new Value(val.asBool(), Type.BOOL);
			case NUMBER -> new Value(val.asNr(), Type.NUMBER);
			default -> throw new AssertionError();
			};
	}

	public Value getValue() {
		return value;
	}

	public Scope getScope() {
		return name.getScope();
	}

	public String getName() {
		return name.getName();
	}

	public Type getType() {
		return null;
	}
}
