package building.expressions.normal.containers;

import static building.types.specific.ExpressionType.NAME;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.Scope;
import building.expressions.abstractions.interfaces.NameHolder;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.types.specific.FlagType;
import building.types.specific.KeywordType;
import building.types.specific.data.ArrayType;
import building.types.specific.data.DataType;
import runtime.datatypes.Value;
import runtime.exceptions.DeclarationException;

/**
 * Every piece of text that isn't predefined by the Interpreter via Keywords, Operators, etc...
 * (Wrapper-{@link Expression} for {@link String}s).
 */
public class Name extends Expression implements ValueChanger {

	private final String name;

	/** Creates a {@link Name} from a {@link String}. */
	public Name(int lineID, String name) {
		super(lineID, NAME);
		this.name = name;
		if (name == null)
			throw new AssertionError("Name cannot be null.");
		if (!isName(name))
			throw new DeclarationException(getOriginalLine(), "The name has to pass the name-check. Was: " + name);
	}

	/** A shortcut for getting the value over {@link Scope#get()}. */
	@Override
	public Value getValue() {
		return getScope().get(name, getOriginalLine()).getValue();
	}

	/** A shortcut for setting the value over the {@link Scope}. */
	@Override
	public void setValue(Value val) {
		getScope().get(name, getOriginalLine()).setValue(val);
	}

	/** Arg is valid name if alphanumerical with underscores. (Atleast one character.) */
	public static boolean isName(String arg) {
		// @formatter:off
		return arg.matches("\\w*([a-z]|[A-Z])+\\w*") 
				&& !KeywordType.isKeyword(arg) 
				&& !DataType.isType(arg) 
				&& !ArrayType.isType(arg)
				&& !FlagType.isFlag(arg);
		// @formatter:on
	}

	/**
	 * Returns the String-Representation of this {@link Name}.
	 * 
	 * This is the base for all instances of {@link NameHolder#getNameString()}.
	 */
	@Override
	public final String getNameString() {
		return name;
	}

	@Override
	public Name getName() {
		return this;
	}

	@Override
	public String toString() {
		return getNameString();
	}
}