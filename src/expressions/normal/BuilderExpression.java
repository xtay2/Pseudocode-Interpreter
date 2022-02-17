package expressions.normal;

import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.MergedExpression;
import types.ExpressionType;
import types.specific.BuilderType;

/**
 * An {@link Expression} that later gets destructed and hasn't any persistency/logic in the code.
 * This acts solely as a part of any {@link MergedExpression}.
 * 
 * @see MergedExpression
 * @see Expression
 */
public class BuilderExpression extends Expression {

	/** Creates a {@link BuilderExpression}. The lineID is set to -1, because its not needed later. */
	private BuilderExpression(BuilderType type) {
		super(-1, type, type.expected);
	}

	@Override
	public final boolean isDefiniteMainExpression() {
		return false;
	}

	/**
	 * Creates a BuilderExpression from a String that isn't a Keyword.
	 * 
	 * @return null if the String doesn't match any {@link ExpressionType}.
	 */
	public static BuilderExpression create(String arg) {
		BuilderType t = isBuilderType(arg);
		return t != null ? new BuilderExpression(t) : null;
	}

	/**
	 * Returns the matching ExpressionType or null if there isnt one. Used in
	 * {@link BuilderExpression#create(String)}.
	 */
	private static BuilderType isBuilderType(String arg) {
		for (BuilderType t : BuilderType.values()) {
			if (t.id.equals(arg))
				return t;
		}
		return null;
	}

}
