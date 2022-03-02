package modules.merger.sub;

import static types.specific.KeywordType.IF;

import exceptions.runtime.UnexpectedTypeError;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.statements.ConditionalStatement;
import expressions.main.statements.ReturnStatement;
import modules.merger.SuperMerger;
import types.specific.KeywordType;

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
