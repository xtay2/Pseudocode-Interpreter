package expressions.special;

import expressions.normal.Expression;
import parsing.program.KeywordType;

/**
 * An {@link Expression} that later gets destructed and hasn't any
 * persistency/logic in the code. This acts solely as a part of any
 * {@link MergedExpression}.
 */
public class BuilderExpression extends Expression {

	private enum Type {
		TO(KeywordType.TO.toString()), STEP(KeywordType.STEP.toString());

		final String keyword;

		Type(String string) {
			this.keyword = string;
		}

		static Type build(String keyword) {
			for (Type v : values())
				if (v.keyword.equals(keyword))
					return v;
			throw new AssertionError(keyword + " is not a BuilderExpression.");
		}
	}

	public final Type type;

	private BuilderExpression(Type t) {
		super(-1);
		this.type = t;
	}

	public static BuilderExpression build(String keyword) {
		return new BuilderExpression(Type.build(keyword));
	}

}
