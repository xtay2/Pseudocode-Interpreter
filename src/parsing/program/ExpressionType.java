package parsing.program;

import expressions.main.Declaration;
import expressions.normal.Comma;
import expressions.normal.ExpectedReturnType;
import expressions.normal.ExpectedType;
import expressions.normal.LoopConnector;
import expressions.normal.Name;
import expressions.normal.array.ArrayEnd;
import expressions.normal.array.ArrayStart;
import expressions.normal.brackets.CloseBracket;
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
	CLOSE_SCOPE("}"),

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
	ASSIGNMENT("="),

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
	 * Verbindungsworte in Schleifen.
	 *
	 * Beispiele: "to" (from 0 to 10)
	 *
	 * @see LoopConnector
	 */
	LOOP_CONNECTOR("to"),
	
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
	OPERATION_ASSIGNMENT("Operation-Declaration");

	private final String name;

	ExpressionType(String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		return name;
	}
}
