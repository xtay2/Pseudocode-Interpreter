package parsing.program;

import expressions.main.CloseBlock;
import expressions.main.Declaration;
import expressions.normal.Comma;
import expressions.normal.ExpectedReturnType;
import expressions.normal.ExpectedType;
import expressions.normal.Literal;
import expressions.normal.LoopConnector;
import expressions.normal.Name;
import expressions.normal.brackets.CloseBracket;
import expressions.normal.brackets.OpenBlock;
import expressions.normal.brackets.OpenBracket;
import expressions.normal.operators.Operator;

public enum ExpressionType {

	/**
	 * Keyword
	 *
	 * @see KeywordType
	 */
	KEYWORD("Keyword"),

	/**
	 * Ausgeschriebener alphanumerischer Name.
	 *
	 * @see Name
	 */
	NAME("Name"),

	/**
	 * Ausgeschriebener Wert.
	 *
	 * @see Literal
	 */
	LITERAL("Literal"),

	/**
	 * Identifier bei Variablendeklaration. var, bool, nr oder text.
	 * 
	 * @see TypedVar
	 */
	VAR_TYPE("Var-Declaration"),

	/**
	 * Identifier bei Parameterdeklaration/Returntype in Funktionen. bool, nr oder
	 * text.
	 *
	 * @see ExpectedType
	 */
	EXPECTED_TYPE("Return-Type"),

	/**
	 * Pfeil ->
	 *
	 * @see ExpectedReturnType
	 */
	EXPECTED_RETURN_TYPE("->"),

	/**
	 * = Zeichen
	 *
	 * @see Declaration
	 */
	DECLARATION("="),

	/**
	 * ( Zeichen
	 *
	 * @see OpenBracket
	 */
	OPEN_BRACKET("("),

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
	 * { Zeichen
	 *
	 * @see OpenBlock
	 */
	OPEN_BLOCK("{"),

	/**
	 * } Zeichen
	 *
	 * @see CloseBlock
	 */
	CLOSE_BLOCK("}"),

	/**
	 * Infixoperatoren wie +, -, and, or, <, !=
	 *
	 * @see Operator
	 */
	INFIX_OPERATOR("Operator"),

	/**
	 * Verbindungsworte in Schleifen.
	 *
	 * Beispiele: "in" (for e in list) oder "to" (from 0 to 10)
	 *
	 * @see LoopConnector
	 */
	LOOP_CONNECTOR("in/to"),

	/**
	 * [ Zeichen. Signalisiert, dass nun etwas kommt, dass mit einem Array zutun
	 * hat.
	 * 
	 * @see ArrayStart
	 * @see Array
	 */
	ARRAY_START("["),

	/**
	 * ] Zeichen. Signalisiert, dass der Arraybereich endet.
	 * 
	 * @see ArrayEnd
	 * @see Array
	 */
	ARRAY_END("]"),
	
	/**
	 * ; Semikolon. Optional nach Funktionsaufrufen.
	 */
	DEFINITE_LINEBREAK(";");
	
	private final String name;
	
	@Override
	public String toString() {
		return name;
	}
	
	ExpressionType(String name) {
		this.name = name;
	}
}
