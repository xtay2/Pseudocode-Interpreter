package building.expressions.normal.containers;

import static building.types.specific.FlagType.CONSTANT;
import static building.types.specific.FlagType.FINAL;
import static runtime.datatypes.object.NullValue.NULL;

import java.util.HashSet;
import java.util.Set;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.Scope;
import building.expressions.abstractions.interfaces.Flaggable;
import building.expressions.abstractions.interfaces.Registerable;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.types.specific.DataType;
import building.types.specific.FlagType;
import runtime.datatypes.Value;
import runtime.datatypes.object.NullValue;
import runtime.exceptions.CastingException;
import runtime.exceptions.DeclarationException;

/**
 * Has a Name and a Value. The Name has a scope.
 *
 * Gets created by keywords like var, bool, nr, text, obj or as a parameter in a function through
 * the {@link DataType}.
 *
 * Gets saved in its {@link Scope} and should only get accessed by it.
 */
public class Variable extends Expression implements Registerable, ValueChanger, Flaggable {

	// DATA
	private final Name name;
	private Value value;

	// FLAGS
	private final Set<FlagType> flags = new HashSet<>();

	/**
	 * Creates and registers a {@link Variable}.
	 * 
	 * @param outer is the outer Scope in which this {@link Registerable} lies.
	 * @param type  is the {@link DataType} of this {@link Variable}.
	 * @param name  is the unique {@link Name} of this {@link Variable}.
	 * @param val   is an optional {@link Variable}. Input null if no value is wanted.
	 * @param flags are optional {@link FlagType}s.
	 */
	public Variable(int lineID, Scope outer, DataType type, Name name, Value val) {
		super(lineID, type);
		if (type == null)
			throw new AssertionError("The type cannot be null.");
		this.name = name;
		outer.register(this);
		setValue(val);
	}

	/**
	 * Returns the {@link Value} of this variable or {@link NullValue#NULL} if it isn't initialised yet.
	 */
	@Override
	public Value getValue() {
		if (value == null)
			return NULL;
		return value;
	}

	/**
	 * Should get identified through {@link Scope#get()}.
	 * 
	 * @throws CastingException if this is a TypedVar and the types don't match.
	 */
	@Override
	public void setValue(Value val) throws CastingException {
		if (val == null)
			throw new AssertionError("Value cannot be null.");
		if (hasFlag(FINAL) || hasFlag(CONSTANT) && value != null)
			throw new DeclarationException(getOriginalLine(),
					"Trying to modify the " + (hasFlag(CONSTANT) ? "constant " : "final variable ") + getName());
		value = val.as((DataType) type);
	}

	@Override
	public Name getName() {
		return name;
	}

	@Override
	public void addFlags(Set<FlagType> flags) {
		this.flags.addAll(flags);
	}

	@Override
	public boolean hasFlag(FlagType f) {
		return flags.contains(f);
	}
}
