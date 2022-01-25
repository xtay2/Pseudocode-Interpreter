package expressions.normal;

import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

import expressions.main.loops.FromToLoop;

/**
 * The Symbol |, used to make a {@link FromToLoop} prettier.
 */
public class LoopConnector extends Expression {

	public LoopConnector(int line) {
		super(line);
		setExpectedExpressions(LITERAL, NAME, ARRAY_START);
	}
}
