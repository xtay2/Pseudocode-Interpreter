package parsing.program;

import expressions.main.CloseBlock;
import expressions.main.Declaration;
import expressions.normal.Comma;
import expressions.normal.ExpectedReturnType;
import expressions.normal.ExpectedType;
import expressions.normal.LoopConnector;
import expressions.normal.Name;
import expressions.normal.array.ArrayEnd;
import expressions.normal.array.ArrayStart;
import expressions.normal.brackets.CloseBracket;
import expressions.normal.brackets.OpenBlock;
import expressions.normal.brackets.OpenBracket;
import expressions.normal.operators.Operator;

public enum ExpressionType {

	/**
	 * ] Zeichen. Signalisiert, dass der Arraybereich endet.
	 * 
	 * @see ArrayEnd
	 * @see Array
	 */
	ARRAY_END("]"),

	/**
	 * [ Zeichen. Signalisiert, dass nun etwas kommt, dass mit einem Array zutun
	 * hat.
	 * 
	 * @see ArrayStart
	 * @see Array
	 */
	ARRAY_START("["),

	/**
	 * } Zeichen
	 *
	 * @see CloseBlock
	 */
	CLOSE_BLOCK("}"),

	/**
	 * ) Zeichen
	 *
	 * @see CloseBracket
	 */
	CLOSE_BRACKET(")"),

	/**
	 * , Zeichen
	 *
	 * @see Comma
	 */
	COMMA("','"),

	/**
	 * Pre- or Post- In- or Decrement.
	 * 
	 *  x++, ++x, x--, --x
	 */
	CREMENT("Crement"),

	/**
	 * = Zeichen
	 *
	 * @see Declaration
	 */
	DECLARATION("="),

	/**
	 * ; Semikolon. Optional nach Funktionsaufrufen.
	 */
	DEFINITE_LINEBREAK(";"),

	/**
	 * Pfeil ->
	 *
	 * @see ExpectedReturnType
	 */
	EXPECTED_RETURN_TYPE("->"),

	/**
	 * Identifier bei Parameterdeklaration/Returntype in Funktionen. bool, nr oder
	 * text.
	 *
	 * @see ExpectedType
	 */
	EXPECTED_TYPE("Return-Type"),

	/**
	 * Infixoperatoren wie +, -, and, or, <, !=
	 *
	 * @see Operator
	 */
	INFIX_OPERATOR("Operator"),

	/**
	 * Keyword
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
	 * Verbindungsworte in Schleifen.
	 *
	 * Beispiele: "in" (for e in list) oder "to" (from 0 to 10)
	 *
	 * @see LoopConnector
	 */
	LOOP_CONNECTOR("in/to"),

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
	OPEN_BLOCK("{"),

	/**
	 * ( Zeichen
	 *
	 * @see OpenBracket
	 */
	OPEN_BRACKET("("),

	/**
	 * +=, -=, *=, /=, %=, ^=
	 * 
	 * @see OperationDeclaration
	 */
	OPERATION_ASSIGNMENT("Operation-Declaration"),

	/**
	 * Identifier bei Variablendeklaration. var, bool, nr oder text.
	 * 
	 * @see TypedVar
	 */
	VAR_TYPE("Var-Declaration");

	private final String name;

	ExpressionType(String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		return name;
	}
}
