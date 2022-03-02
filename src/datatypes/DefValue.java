package datatypes;

import static types.specific.data.DataType.DEF;

import exceptions.runtime.UnexpectedTypeError;
import expressions.abstractions.interfaces.Callable;
import expressions.abstractions.interfaces.NameHolder;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.functions.Returnable;
import expressions.normal.containers.Name;
import modules.interpreter.FuncManager;
import types.specific.data.DataType;

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
