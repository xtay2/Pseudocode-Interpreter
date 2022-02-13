package parsing.program;

import expressions.abstractions.MergedExpression;
import expressions.main.Declaration;
import expressions.normal.ExpectedType;
import expressions.normal.containers.Name;
import expressions.normal.operators.Operator;
import expressions.possible.Assignment;

public enum ExpressionType {

	// BUILDER EXPRESSIONS (String has to match code.)

	ARRAY_END("]"), ARRAY_START("["),

	OPEN_BRACKET("("), CLOSE_BRACKET(")"),

	EXPECTED_RETURN_TYPE("->"), COMMA(","),

	MULTI_CALL_LINE("|"),

	// NON-BUILDER EXPRESSIONS

	/**
	 * Pre- or Post- In- or Decrement.
	 * 
	 * x++, ++x, x--, --x
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
	 * Identifier bei Parameterdeklaration/Returntype in Funktionen. bool, nr oder
	 * text.
	 *
	 * @see ExpectedType
	 */
	EXPECTED_TYPE("Data-Type"),

	/**
	 * Infixoperatoren wie +, -, and, or, <, !=
	 *
	 * @see Operator
	 */
	INFIX_OPERATOR("Operator"),

	/**
	 * Keywords wie if, for, func...
	 * 
	 * @see KeywordType
	 */
	KEYWORD("Keyword"),

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
	OPERATION_ASSIGNMENT("Operation-Declaration"), 
	
	/**
	 * Any {@link MergedExpression}
	 */
	MERGED("Merged");

	private final String name;

	ExpressionType(String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		return name;
	}
}
