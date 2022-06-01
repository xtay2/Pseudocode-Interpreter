package building.expressions.normal.containers;

import static building.types.specific.FlagType.CONSTANT;
import static building.types.specific.FlagType.FINAL;
import static runtime.datatypes.MaybeValue.NULL;

import java.util.HashSet;
import java.util.Set;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.Scope;
import building.expressions.abstractions.interfaces.Flaggable;
import building.expressions.abstractions.interfaces.Registerable;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.types.specific.BuilderType;
import building.types.specific.FlagType;
import building.types.specific.datatypes.DataType;
import errorhandeling.NonExpressionException;
import errorhandeling.PseudocodeException;
import runtime.datatypes.MaybeValue;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;

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
	private MaybeValue value;
	public final DataType dataType;

	// FLAGS
	private final Set<FlagType> flags = new HashSet<>();

	/**
	 * Creates and registers a {@link Variable}.
	 *
	 * @param outer is the outer Scope in which this {@link Registerable} lies.
	 * @param type is the {@link DataType} of this {@link Variable}.
	 * @param name is the unique {@link Name} of this {@link Variable}.
	 * @param val is an optional {@link Variable}. Input null if no value is wanted.
	 */
	public Variable(int lineID, Scope outer, DataType dataType, Name name, Value val) {
		super(lineID, BuilderType.MERGED);
		assert dataType != null : "The type of a variable cannot be null.";
		assert name != null : "The name of a variable cannot be null.";
		this.dataType = dataType;
		this.name = name;
		outer.register(this);
		setValue(val);
	}

	/**
	 * Returns the {@link Value} of this variable or {@link NullValue#NULL} if it isn't initialised yet.
	 */
	@Override
	public Value getValue() {
		return value.getValue();
	}

	/**
	 * Should get identified through {@link Scope#get()}.
	 *
	 * @throws NonExpressionException if this is a TypedVar and the types don't match.
	 */
	@Override
	public Value setValue(Value val) {
		if (val == null)
			throw new AssertionError("Value cannot be null.");
		if (hasFlag(FINAL) || hasFlag(CONSTANT) && value != null)
			throw new PseudocodeException("ImmutableModification",
					"Trying to modify the " + (hasFlag(CONSTANT) ? "constant " : "final variable ") + getName(), getDataPath());
		if (!allowsNull() && val == NULL) {
			throw new PseudocodeException("NullNotAllowed",
					"This variable doesn't allow null. To change that, write:\n" + dataType + "" + BuilderType.MAYBE + " ...",
					getDataPath());
		}
		if (!val.matches(dataType)) {
			String passedType = (val instanceof ArrayValue at ? at.getRules() : val.dataType).toString();
			throw new PseudocodeException("UnexpectedDataType", //
					"Variable \"" + getNameString() + "\" only expects values of type " + dataType + ", but this was passed instead:"
							+ "\nValue: " + val + "\nType: " + passedType,
					getDataPath());
		}
		value = new MaybeValue(val);
		return getValue();
	}

	public boolean allowsNull() {
		return dataType.allowsNull;
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
