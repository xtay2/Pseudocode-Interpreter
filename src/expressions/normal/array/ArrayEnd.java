package expressions.normal.array;

import static parsing.program.ExpressionType.ARRAY_END;
import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.CLOSE_BRACKET;
import static parsing.program.ExpressionType.COMMA;
import static parsing.program.ExpressionType.CREMENT;
import static parsing.program.ExpressionType.DECLARATION;
import static parsing.program.ExpressionType.DEFINITE_LINEBREAK;
import static parsing.program.ExpressionType.INFIX_OPERATOR;
import static parsing.program.ExpressionType.NAME;
import static parsing.program.ExpressionType.OPEN_BLOCK;

import expressions.special.Expression;
import helper.Output;

public class ArrayEnd extends Expression {

	public ArrayEnd(int line) {
		super(line);
		setExpectedExpressions(COMMA, CLOSE_BRACKET, DECLARATION, NAME, INFIX_OPERATOR, ARRAY_END, ARRAY_START, OPEN_BLOCK,
				DEFINITE_LINEBREAK, CREMENT);
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "']'";
	}
}
