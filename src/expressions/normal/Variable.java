package expressions.normal;

import datatypes.Value;
import exceptions.runtime.CastingException;
import expressions.special.Expression;
import expressions.special.Scope;
import expressions.special.Type;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.VarManager;
import parser.program.ExpressionType;

/**
 * Has a Name and a Value. The Name has a scope.
 *
 * @see TypedVar
 */
public class Variable extends Expression implements ValueHolder {

	private final Type type;
	private Name name;
	private Value value = null;

	public Variable(int line, Type type) {
		super(line);
		this.type = type;
		setExpectedExpressions(ExpressionType.NAME, ExpressionType.ARRAY_START);
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
		value = val.as(type);
	}

	@Override
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
		return type;
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : (type == null ? "Var" : type.getName() + "-Var");
	}
}
