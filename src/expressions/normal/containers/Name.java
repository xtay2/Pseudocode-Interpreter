package expressions.normal.containers;

import static types.SuperType.*;
import static types.specific.BuilderType.ARRAY_END;
import static types.specific.BuilderType.ARRAY_START;
import static types.specific.BuilderType.CLOSE_BRACKET;
import static types.specific.BuilderType.COMMA;
import static types.specific.BuilderType.MULTI_CALL_LINE;
import static types.specific.BuilderType.OPEN_BRACKET;
import static types.specific.ExpressionType.ASSIGNMENT;
import static types.SuperType.INFIX_OPERATOR;
import static types.specific.ExpressionType.NAME;
import static types.specific.ExpressionType.OPEN_SCOPE;
import static types.specific.ExpressionType.OPERATION_ASSIGNMENT;

import datatypes.Value;
import exceptions.runtime.DeclarationException;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueChanger;
import helper.Output;
import modules.interpreter.VarManager;
import types.specific.DataType;
import types.specific.FlagType;
import types.specific.KeywordType;

/**
 * Every piece of text that isn't predefined by the Interpreter via Keywords, Operators, etc...
 * 
 * This is not a {@link MergedExpression}, because the functionality can be reduced into the
 * constructor. (Wrapper-{@link Expression} for {@link String}s).
 */
public class Name extends Expression implements ValueChanger {

	private final String name;

	/** Creates a {@link Name} from a {@link String}. */
	public Name(int line, String name) {
		super(line, NAME, ASSIGNMENT, OPERATION_ASSIGNMENT, OPEN_BRACKET, COMMA, CLOSE_BRACKET, OPEN_SCOPE, INFIX_OPERATOR, ARRAY_START,
				ARRAY_END, POSTFIX_OPERATOR, KEYWORD_TYPE, MULTI_CALL_LINE);
		if (!isName(name))
			throw new DeclarationException(getOriginalLine(), "The name has to pass the name-check. Was: " + name);
		this.name = name;
	}

	/** Returns the String-Representation. */
	public String getName() {
		return name;
	}

	/** A shortcut for getting the value over the {@link VarManager}. */
	@Override
	public Value getValue() {
		return VarManager.get(name, getOriginalLine()).getValue();
	}

	/** A shortcut for setting the value over the {@link VarManager}. */
	@Override
	public void setValue(Value val) {
		VarManager.get(name, getOriginalLine()).setValue(val);
	}

	/** Arg is valid name if alphanumerical with underscores. (Atleast one character.) */
	public static boolean isName(String arg) {
		return arg.matches("\\w*([a-z]|[A-Z])+\\w*") && !KeywordType.isKeyword(arg) && !DataType.isType(arg) && !FlagType.isFlag(arg);
	}

	@Override
	public String toString() {
		return Output.DEBUG ? getClass().getSimpleName() : name;
	}
}
