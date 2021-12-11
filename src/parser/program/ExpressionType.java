package parser.program;

import expressions.main.CloseBlock;
import expressions.main.Declaration;
import expressions.normal.CloseBracket;
import expressions.normal.Comma;
import expressions.normal.ExpectedReturnType;
import expressions.normal.ExpectedType;
import expressions.normal.Literal;
import expressions.normal.LoopConnector;
import expressions.normal.Name;
import expressions.normal.OpenBlock;
import expressions.normal.OpenBracket;
import expressions.normal.operators.Operator;

public enum ExpressionType {

	/**
	 * Keyword
	 *
	 * @see KeywordType
	 */
	KEYWORD,

	/**
	 * Ausgeschriebener alphanumerischer Name.
	 *
	 * @see Name
	 */
	NAME,

	/**
	 * Ausgeschriebener Wert.
	 *
	 * @see Literal
	 */
	LITERAL,

	/**
	 * Identifier bei Variablendeklaration. var, bool, nr oder text.
	 * 
	 * @see TypedVar
	 */
	VAR_TYPE,

	/**
	 * Identifier bei Parameterdeklaration/Returntype in Funktionen. bool, nr oder
	 * text.
	 *
	 * @see ExpectedType
	 */
	EXPECTED_TYPE,

	/**
	 * Pfeil ->
	 *
	 * @see ExpectedReturnType
	 */
	EXPECTED_RETURN_TYPE,

	/**
	 * = Zeichen
	 *
	 * @see Declaration
	 */
	DECLARATION,

	/**
	 * ( Zeichen
	 *
	 * @see OpenBracket
	 */
	OPEN_BRACKET,

	/**
	 * ) Zeichen
	 *
	 * @see CloseBracket
	 */
	CLOSE_BRACKET,

	/**
	 * , Zeichen
	 *
	 * @see Comma
	 */
	COMMA,

	/**
	 * { Zeichen
	 *
	 * @see OpenBlock
	 */
	OPEN_BLOCK,

	/**
	 * } Zeichen
	 *
	 * @see CloseBlock
	 */
	CLOSE_BLOCK,

	/**
	 * : Zeichen. Signalisiert, dass nur die eine darauffolgende Zeile zum Statement
	 * gehört.
	 */
	ONE_LINE_STATEMENT,

	/**
	 * Infixoperatoren wie +, -, and, or, <, !=
	 *
	 * @see Operator
	 */
	INFIX_OPERATOR,

	/**
	 * Verbindungsworte in Schleifen.
	 *
	 * Beispiele: "in" (for e in list) oder "to" (from 0 to 10)
	 *
	 * @see LoopConnector
	 */
	LOOP_CONNECTOR,
	
	/**
	 * [ Zeichen.
	 * Signalisiert, dass nun etwas kommt, dass mit einem Array zutun hat.
	 * 
	 * @see ArrayStart
	 * @see Array
	 */
	ARRAY_START,
	
	/**
	 * ] Zeichen.
	 * Signalisiert, dass der Arraybereich endet.
	 * 
	 * @see ArrayEnd
	 * @see Array
	 */
	ARRAY_END;
}
