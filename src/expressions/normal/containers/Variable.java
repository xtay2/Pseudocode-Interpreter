package expressions.normal.containers;

import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.NAME;

import datatypes.Value;
import exceptions.runtime.CastingException;
import expressions.abstractions.Expression;
import expressions.abstractions.Scope;
import expressions.abstractions.ValueChanger;
import expressions.normal.ExpectedType;
import expressions.possible.Call;
import expressions.special.DataType;
import interpreter.VarManager;
import parsing.program.ExpressionType;
import parsing.program.KeywordType;

/**
 * Has a Name and a Value. The Name has a scope.
 *
 * Gets created by keywords like var, bool, nr, text, obj or as a parameter in a
 * function through the {@link ExpectedType}.
 *
 * Gets saved in the {@link VarManager} and should only get accessed by it.
 */
public class Variable extends Expression implements ValueChanger {

	private Name name;
	private final DataType type;
	private Value value = null;

	/** Initialise a Variable */
	public Variable(int line, DataType type, Name name) {
		super(line, ExpressionType.MERGED);
		setExpectedExpressions(NAME, ARRAY_START);
		this.type = type;
		this.name = name;
	}

	/**
	 * Initialise a Variable with an inital Value. Used in {@link Call} and
	 * {@link VarManager}.
	 */
	public Variable(int line, DataType type, Name name, Value val) {
		this(line, type, name);
		setValue(val);
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
		if (value == null)
			throw new AssertionError("Value cannot be null.");
		return value;
	}

	/**
	 * Should get identified through by {@link VarManager}.
	 * 
	 * @throws CastingException if this is a TypedVar and the types don't match.
	 */
	@Override
	public void setValue(Value val) throws CastingException {
		if (val == null)
			throw new AssertionError("Value cannot be null.");
		value = val.as(type);
	}
}
