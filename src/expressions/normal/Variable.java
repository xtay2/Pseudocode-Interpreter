package expressions.normal;

import datatypes.Castable;
import exceptions.CastingException;
import expressions.special.Expression;
import expressions.special.Scope;
import expressions.special.Type;
import expressions.special.ValueHolder;
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
	private Castable value = null;

	public Variable(int line, Type type) {
		super(line);
		this.type = type;
		setExpectedExpressions(ExpressionType.NAME, ExpressionType.ARRAY_START);
	}

	/** Gets called when declared through Declaration or params in Function. */
	public void initialise(Name name, Castable val) {
		this.name = name;
		setValue(val);
		VarManager.registerVar(this);
	}

	/** Should only get called by VarManager */
	public void setValue(Castable val) throws CastingException {
		if (val == null)
			throw new NullPointerException("Value cannot be null.");
		value = val.as(type);
	}

	@Override
	public Castable getValue() {
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
		return type == null ? "Var" : type.getName() + "-Var";
	}
}
