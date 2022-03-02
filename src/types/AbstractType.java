package types;

import static types.SuperType.EXPECTED_TYPE;
import static types.SuperType.EXPRESSION_TYPE;
import static types.specific.BuilderType.ARRAY_START;
import static types.specific.BuilderType.OPEN_BRACKET;

import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.BuilderExpression;
import modules.finder.ExpressionFinder;
import modules.parser.program.ProgramLine;

/**
 * This is the Super-Interface for all Types. It gets used in every {@link Expression}.
 */
public interface AbstractType {

	/**
	 * Use this when any kind of {@link ValueHolder} is expected.
	 * 
	 * [LITERAL, NAME, ARRAY_START, OPEN_BRACKET]
	 */
	static final AbstractType[] VAL_HOLDER_TYPES = { EXPRESSION_TYPE, ARRAY_START, OPEN_BRACKET, EXPECTED_TYPE };

	/**
	 * Used to create an Expression from this {@link AbstractType} and a {@link String}.
	 * 
	 * This gets called in the {@link ExpressionFinder}.
	 * 
	 * @param arg     is the String.
	 * @param lineID  is the line-identifier of the calling {@link ProgramLine}.
	 * @param myScope is the scope, the resulting Expression lies in.
	 * 
	 * @return null if the {@link String} didn't match this type.
	 * @return the {@link Expression} or {@code null} if the {@link String} doesn't match.
	 */
	public default BuilderExpression create(String arg, int lineID) {
		if (!toString().equals(arg.strip()))
			return null;
		return new BuilderExpression(lineID, this);
	}

	/** Checks if this Type is the Subtype of the given {@link SuperType}. */
	boolean is(SuperType superType);

	/** Returns an array of expected following types. Gets called by the {@link BuilderExpression}. */
	AbstractType[] expected();
}
