package expressions.normal;

import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.NAME;

import datatypes.Value;
import exceptions.runtime.CastingException;
import expressions.special.DataType;
import expressions.special.Expression;
import expressions.special.Scope;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.VarManager;

/**
 * Has a Name and a Value. The Name has a scope.
 *
 * Gets created by keywords like var, bool, nr, text, obj or as a parameter in a
 * function.
 *
 * Gets saved in the {@link VarManager} and should only get accessed by it.
 *
 * Gets inherited by:
 * 
 *
 */
public class Variable extends Expression implements ValueHolder {

	private Name name;
	private final DataType type;
	private Value value = null;

	public Variable(int line, DataType type) {
		super(line);
		this.type = type;
		setExpectedExpressions(NAME, ARRAY_START);
	}

	public String getName() {
		return name.getName();
	}

	public Scope getScope() {
		return name.getScope();
	}

	public DataType getType() {
		return type;
	}

	@Override
	public Value getValue() {
		return value;
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
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : (type == null ? "Var" : type.getName() + "-Var");
	}
}
