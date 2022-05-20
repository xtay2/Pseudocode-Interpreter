package building.expressions.normal.containers;

import static building.types.specific.DynamicType.NAME;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.Scope;
import building.expressions.abstractions.interfaces.NameHolder;
import building.expressions.abstractions.interfaces.ValueChanger;
import building.types.abstractions.SpecificType;
import building.types.specific.FlagType;
import building.types.specific.KeywordType;
import building.types.specific.datatypes.SingleType;
import building.types.specific.operators.InfixOpType;
import formatter.basic.Formatter;
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
		assert name != null : "Name cannot be null.";
		if (!isName(name) && !KeywordType.MAIN.toString().equals(name))
			throw new DeclarationException(getOriginalLine(), "The name has to pass the name-check. Was: " + name);
	}

	/** A shortcut for getting the value over {@link Scope}. */
	@Override
	public Value getValue() {
		return getScope().getVar(name, getOriginalLine()).getValue();
	}

	/** A shortcut for setting the value over the {@link Scope}. */
	@Override
	public Value setValue(Value val) {
		return getScope().getVar(name, getOriginalLine()).setValue(val);
	}

	/** Arg is valid name if alphanumerical with underscores. (Atleast one character.) */
	public static boolean isName(String arg) {
		return arg.matches(Formatter.WR) && !isAlphaNumKeyword(arg);
	}

	/** Returns true, if the passed string matches any alphanumerical keyword. */
	public static boolean isAlphaNumKeyword(String arg) {
		//@formatter:off
		return SpecificType.equalsString(arg, KeywordType.class)
				|| SpecificType.equalsString(arg, SingleType.class)
				|| SpecificType.equalsString(arg, FlagType.class)
				|| SpecificType.equalsString(arg, InfixOpType.class);
		//@formatter:on
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
	public boolean equals(Object obj) {
		return obj instanceof Name n ? name.equals(n.name) : false;
	}

	@Override
	public String toString() {
		return getNameString();
	}
}
