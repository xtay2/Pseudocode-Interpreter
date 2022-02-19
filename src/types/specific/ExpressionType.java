package types.specific;

import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.main.CloseScope;
import expressions.main.Declaration;
import expressions.main.OperationAssignment;
import expressions.normal.brackets.OpenScope;
import expressions.normal.containers.Name;
import expressions.normal.operators.Operator;
import expressions.possible.Assignment;
import expressions.possible.Crement;
import types.AbstractType;
import types.SuperType;

public enum ExpressionType implements AbstractType {

	/**
	 * Every {@link MergedExpression} that doesn't get specified by any {@link KeywordType}.
	 */
	MERGED("Merged"),

	/**
	 * Pre- or Post- Increment.
	 * 
	 * x++, ++x
	 * 
	 * @see Crement
	 */
	INCREMENT("Increment"),

	/**
	 * Pre- or Post- Decrement.
	 * 
	 * x--, --x
	 * 
	 * @see Crement
	 */
	DECREMENT("Decrement"),

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

	@Override
	public Expression create(String arg, int lineID) {
		return switch (this) {
			// Direct build
			case ASSIGNMENT -> "=".equals(arg) ? new Assignment(lineID) : null;
			case INCREMENT -> "++".equals(arg) ? new Crement(INCREMENT, lineID) : null;
			case DECREMENT -> "--".equals(arg) ? new Crement(DECREMENT, lineID) : null;
			case OPEN_SCOPE -> "{".equals(arg) ? new OpenScope(lineID) : null;
			case CLOSE_SCOPE -> "}".equals(arg) ? new CloseScope(lineID) : null;
			case NAME -> Name.isName(arg) ? new Name(lineID, arg) : null;
			// External build
			case OPERATION_ASSIGNMENT -> OperationAssignment.stringToOpAssign(arg, lineID);
			case INFIX_OPERATOR -> Operator.stringToOperator(arg, lineID);
			case LITERAL -> Value.stringToLiteral(arg);
			// Not supported
			case MERGED -> throw new AssertionError("Types cannot be merged at this point.");
		};
	}

	@Override
	public boolean is(SuperType superType) {
		return superType == SuperType.EXPRESSION_TYPE;
	}
}
