package building.expressions.abstractions.interfaces;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.MainExpression;
import building.types.abstractions.AbstractType;
import importing.filedata.paths.DataPath;

/**
 * This is a super-interface for every interface that is implicitly an {@link Expression}.
 */
public interface AbstractExpression {

	/** Returns the {@link DataPath} of this {@link Expression}. */
	public DataPath getDataPath();

	/**
	 * Tells, if a specific {@link Expression} is a {@link MainExpression}.
	 */
	public boolean isDefiniteMainExpression();

	/*
	 * Used mostly in the {@link SuperMerger} for any {@link BuilderExpression} to assure, that this
	 * Expression is of a certain {@link KeywordType}, when instanceof is no option.
	 */
	public boolean is(AbstractType type);
}
