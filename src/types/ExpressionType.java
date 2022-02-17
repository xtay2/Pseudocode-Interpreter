package types;

import expressions.abstractions.interfaces.MergedExpression;
import expressions.main.Declaration;
import expressions.normal.containers.Name;
import expressions.normal.operators.Operator;
import expressions.possible.Assignment;
import expressions.possible.Crement;
import types.specific.BuilderType;
import types.specific.DataType;
import types.specific.FlagType;
import types.specific.KeywordType;

public enum ExpressionType implements AbstractType {

	/** Every {@link SpecificType}.*/

	/** {@link BuilderType} like arraystart or comma. */
	BUILDER_TYPE("BuilderType"),
	
	/** {@link DataType} like nr or bool. */
	DATA_TYPE("DataType"),

	/** {@link KeywordType} like if or for. */
	KEYWORD("KeywordType"),

	/** {@link FlagType} like native or const. */
	FLAG("FlagType"),

	/**
	 * Every {@link MergedExpression} that doesn't get specified by any
	 * {@link KeywordType}.
	 */
	MERGED("Merged"),
	
	/**
	 * Pre- or Post- In- or Decrement.
	 * 
	 * x++, ++x, x--, --x
	 * 
	 * @see Crement
	 */
	CREMENT("Crement"),

	/**
	 * = Zeichen
	 *
	 * @see Declaration
	 * @see Assignment
	 */
	ASSIGNMENT("="),

	/**
	 * Infixoperatoren wie +, -, and, or, <, !=
	 *
	 * @see Operator
	 */
	INFIX_OPERATOR("Operator"),

	/**
	 * Ausgeschriebener Wert.
	 *
	 * @see Literal
	 */
	LITERAL("Literal"),

	/**
	 * Ausgeschriebener alphanumerischer Name.
	 *
	 * @see Name
	 */
	NAME("Name"),

	/**
	 * { Zeichen
	 *
	 * @see OpenBlock
	 */
	OPEN_SCOPE("{"),

	/**
	 * } Zeichen
	 *
	 * @see CloseBlock
	 */
	CLOSE_SCOPE("}"),

	/**
	 * +=, -=, *=, /=, %=, ^=
	 * 
	 * @see OperationDeclaration
	 */
	OPERATION_ASSIGNMENT("Operation-Declaration");

	private final String expression;

	ExpressionType(String expression) {
		this.expression = expression;
	}

	@Override
	public String toString() {
		return expression;
	}
}
