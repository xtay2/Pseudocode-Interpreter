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
import expressions.normal.TypedVar;
import expressions.normal.operators.Operator;

public enum ExpressionType {

	/**
	 * Erwartet Name
	 *
	 * @see KeywordType
	 */
	KEYWORD,

	/**
	 * Erwartet Deklaration, Open_Bracket, Comma, Close_Bracket, One_Line_Statement,
	 * Open_Block, Infix_Operator
	 *
	 * @see Name
	 */
	NAME,

	/**
	 * Ausgeschriebener Wert. Erwartet Comma, Close_Bracket, One_Line_Statement,
	 * Infix_Operator
	 *
	 * @see Literal
	 */
	LITERAL,

	/**
	 * Identifier bei Variablendeklaration. bool, nr oder text. Erwartet Name
	 *
	 * @see TypedVar
	 */
	TYPED_VAR,

	/**
	 * Identifier bei Parameterdeklaration/Returntype in Funktionen. bool, nr oder
	 * text. Erwartet Name, Open_Block
	 *
	 * @see ExpectedType
	 */
	EXPECTED_TYPE,

	/**
	 * Pfeil -> Erwartet Expected_Type
	 *
	 * @see ExpectedReturnType
	 */
	EXPECTED_RETURN_TYPE,

	/**
	 * = Zeichen Erwartet Literal, Name
	 *
	 * @see Declaration
	 */
	DECLARATION,

	/**
	 * ( Zeichen Erwartet Close_Bracket, Literal, Name, Var_Type
	 *
	 * @see OpenBracket
	 */
	OPEN_BRACKET,

	/**
	 * ) Zeichen Erwartet Close_Bracket, One_Line_Statement, Comma, Open_Block,
	 * Expected_Return_Type, Infix_Operator
	 *
	 * @see CloseBracket
	 */
	CLOSE_BRACKET,

	/**
	 * , Zeichen Erwartet Name, Literal, Var_Type
	 *
	 * @see Comma
	 */
	COMMA,

	/**
	 * { Zeichen Erwartet Close_Block
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
	 * Infixoperatoren wie +, -, *, / Erwartet Name, Literal.
	 *
	 * @see Operator
	 */
	INFIX_OPERATOR,

	/**
	 * Verbindungsworte in Schleifen. Erwartet Name, Literal.
	 *
	 * Beispiele: "in" (for e in list) oder "to" (from 0 to 10)
	 *
	 * @see LoopConnector
	 */
	LOOP_CONNECTOR;
}
