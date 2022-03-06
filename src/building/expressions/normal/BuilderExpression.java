package building.expressions.normal;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.MainExpression;
import building.types.AbstractType;
import building.types.specific.ExpressionType;

/**
 * An {@link Expression} that later gets destructed and hasn't any persistency in the code.
 * 
 * @see Expression
 * @see MainExpression
 */
public class BuilderExpression extends Expression {

	/** Optional value for {@link ExpressionType#LITERAL} and {@link ExpressionType#NAME}. */
	public final String value;
	private final AbstractType[] expected;

	private BuilderExpression(int lineID, AbstractType type, String value, AbstractType... expected) {
		super(lineID, type);
		this.value = value;
		this.expected = expected;
		if (type == null || expected == null)
			throw new AssertionError("Type and Expected cannot be null. Type was: " + type + ", Exp was: " + expected + ".");
	}

	/** Creates a {@link BuilderExpression} from a {@link AbstractType}. */
	public BuilderExpression(int lineID, AbstractType t) {
		this(lineID, t, null, t.expected());
	}

	/**
	 * Creates a {@link BuilderExpression} from a {@link ExpressionType}.
	 * 
	 * An optional value can be passed. (For {@link ExpressionType#LITERAL}
	 * and{@link ExpressionType#NAME}).
	 */
	public BuilderExpression(int lineID, ExpressionType t, String value) {
		this(lineID, t, value, t.expected());
	}

	@Override
	public final boolean isDefiniteMainExpression() {
		return false;
	}

	public final AbstractType[] getExpectedExpressions() {
		return expected;
	}

	@Override
	public final String toString() {
		return type.toString();
	}
}
