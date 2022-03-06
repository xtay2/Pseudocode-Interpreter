package interpreting.modules.merger;

import static building.types.specific.KeywordType.IF;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.statements.ConditionalStatement;
import building.expressions.main.statements.ReturnStatement;
import building.types.specific.KeywordType;
import runtime.exceptions.UnexpectedTypeError;

/**
 * Builds every Statement, such as {@link ConditionalStatement} and {@link ReturnStatement}.
 */
public abstract class StatementMerger extends SuperMerger {

	/** [IF/ELIF/ELSE] [?BOOL] [OPEN_SCOPE] */
	public static ConditionalStatement buildConditional(KeywordType type) {
		line.remove(0);
		switch (type) {
			case IF, ELIF:
				return new ConditionalStatement(lineID, type, buildVal(), buildOpenScope());
			case ANY:
				if (line.get(0).is(IF)) { // Any-If with condition
					line.remove(0); // IF
					return new ConditionalStatement(lineID, type, buildVal(), buildOpenScope());
				} else // Any without condition
					return new ConditionalStatement(lineID, type, null, buildOpenScope());
			case ELSE:
				return new ConditionalStatement(lineID, type, null, buildOpenScope());
			default:
				throw new UnexpectedTypeError(orgLine, type);
		}
	}

	public static ReturnStatement buildReturn() {
		line.remove(0);
		return new ReturnStatement(lineID, (ValueHolder) build());
	}
}
