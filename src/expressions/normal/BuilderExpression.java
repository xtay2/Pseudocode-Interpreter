package expressions.normal;

import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.MergedExpression;
import types.AbstractType;
import types.specific.BuilderType;
import types.specific.FlagType;
import types.specific.data.ExpectedType;

/**
 * An {@link Expression} that later gets destructed and hasn't any persistency in the code. This
 * acts solely as a part of any {@link MergedExpression}.
 * 
 * @see MergedExpression
 * @see Expression
 */
public class BuilderExpression extends Expression {

	/** The lineID gets set to -1, because it is irrelevant at runtime. */
	private BuilderExpression(AbstractType type, AbstractType... expected) {
		super(-1, type, expected);
		if (type == null)
			throw new AssertionError("Type cannot be null");
	}

	/** Creates a {@link BuilderExpression} from a {@link BuilderType}. */
	public BuilderExpression(BuilderType t) {
		this(t, t.expected);
	}

	/** Creates a {@link BuilderExpression} from a {@link FlagType}. */
	public BuilderExpression(FlagType t) {
		this(t, t.expected);
	}

	/** Creates a {@link BuilderExpression} from an {@link ExpectedType}. */
	public BuilderExpression(ExpectedType t) {
		this(t, ExpectedType.getExpected());
	}

	@Override
	public final boolean isDefiniteMainExpression() {
		return false;
	}

	@Override
	public final String toString() {
		return type.toString();
	}
}
