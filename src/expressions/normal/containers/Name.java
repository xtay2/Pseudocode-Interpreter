package expressions.normal.containers;

import static types.SuperType.*;
import static types.specific.BuilderType.*;
import static types.specific.ExpressionType.NAME;
import static types.specific.ExpressionType.OPEN_SCOPE;

import datatypes.Value;
import exceptions.runtime.DeclarationException;
import expressions.abstractions.Expression;
import expressions.abstractions.Scope;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.NameHolder;
import expressions.abstractions.interfaces.ValueChanger;
import helper.Output;
import types.specific.FlagType;
import types.specific.KeywordType;
import types.specific.data.ArrayType;
import types.specific.data.DataType;

/**
 * Every piece of text that isn't predefined by the Interpreter via Keywords, Operators, etc...
 * 
 * This is not a {@link MergedExpression}, because the functionality can be reduced into the
 * constructor. (Wrapper-{@link Expression} for {@link String}s).
 */
public class Name extends Expression implements ValueChanger {

	private final String name;

	/** Creates a {@link Name} from a {@link String}. */
	public Name(int lineID, String name) {
		super(lineID, NAME, ASSIGNMENT_TYPE, OPEN_BRACKET, COMMA, CLOSE_BRACKET, OPEN_SCOPE, INFIX_OPERATOR, ARRAY_START, ARRAY_END,
				POSTFIX_OPERATOR, KEYWORD_TYPE, MULTI_CALL_LINE, BUILDER_TYPE);
		if (!isName(name))
			throw new DeclarationException(getOriginalLine(), "The name has to pass the name-check. Was: " + name);
		this.name = name;
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
		return Output.DEBUG ? getClass().getSimpleName() : name;
	}
}
