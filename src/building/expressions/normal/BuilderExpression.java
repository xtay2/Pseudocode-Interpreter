package building.expressions.normal;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.MainExpression;
import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;
import building.types.specific.DynamicType;

/**
 * An {@link Expression} that later gets destructed and hasn't any persistency in the code.
 *
 * @see Expression
 * @see MainExpression
 */
public class BuilderExpression extends Expression {

	/** Optional value for {@link ExpressionType#LITERAL} and {@link ExpressionType#NAME}. */
	public final String value;
	private final SpecificType[] expected;

	private BuilderExpression(int lineID, SpecificType type, String value) {
		super(lineID, type);
		this.value = value;
		this.expected = type.expected();
		if (type == null || expected == null)
			throw new AssertionError("Type and Expected cannot be null. Type was: " + type + ", Exp was: " + expected + ".");
	}

	/** Creates a {@link BuilderExpression} from a {@link AbstractType}. */
	public BuilderExpression(int lineID, SpecificType t) {
		this(lineID, t, null);
	}

	/**
	 * Creates a {@link BuilderExpression} from a {@link DynamicType}.
	 *
	 * An optional value can be passed. (For {@link ExpressionType#LITERAL}
	 * and{@link ExpressionType#NAME}).
	 */
	public BuilderExpression(int lineID, DynamicType t, String value) {
		this(lineID, (SpecificType) t, value);
	}

	@Override
	public final boolean isDefiniteMainExpression() {
		return false;
	}

	public final SpecificType[] getExpectedExpressions() {
		return expected;
	}

	@Override
	public final String toString() {
		return type.toString();
	}
}
