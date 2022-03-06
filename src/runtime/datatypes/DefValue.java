package runtime.datatypes;

import static building.types.specific.data.DataType.DEF;

import building.expressions.abstractions.interfaces.Callable;
import building.expressions.abstractions.interfaces.NameHolder;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.functions.Returnable;
import building.expressions.normal.containers.Name;
import building.types.specific.data.DataType;
import interpreting.modules.interpreter.FuncManager;
import runtime.exceptions.UnexpectedTypeError;

/**
 * This {@link Value} is a pure reference to a {@link Returnable}.
 * 
 * Casts on this {@link DefValue} just try to cast the reference, rather than a return-value.
 */
public class DefValue extends Value implements NameHolder, Callable {

	/** The underlying definition. */
	private final String defID;

	/**
	 * Creates a {@link DefValue} from a {@link String}-name, by looking it up in the
	 * {@link FuncManager}.
	 */
	public DefValue(String name) {
		super(DEF);
		if (name == null)
			throw new AssertionError("Name cannot be null.");
		defID = name;
	}

	// Casting

	@Override
	public TextValue asText() {
		return new TextValue(getNameString());
	}

	@Override
	public boolean canCastTo(DataType type) {
		return switch (type) {
			case VAR, DEF -> true; // Returns this
			case TEXT -> true; // Returns the name of the function
			case NUMBER -> true; // Always returns NaN
			// Not implemented
			case OBJECT -> false;
			// Not supported
			case BOOL, INT -> false;
		};
	}

	// Comparison

	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		return v == this;
	}

	/** Returns the wrapped {@link Returnable}. */
	@Override
	public Returnable raw() {
		return FuncManager.findFunc(defID);
	}

	@Override
	public Name getName() {
		return raw().getName();
	}

	@Override
	public Value call(ValueHolder... params) {
		return raw().call(params);
	}
}
