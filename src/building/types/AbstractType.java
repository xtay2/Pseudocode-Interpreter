package building.types;

import static building.types.SuperType.EXPECTED_TYPE;
import static building.types.SuperType.EXPRESSION_TYPE;
import static building.types.specific.BuilderType.ARRAY_START;
import static building.types.specific.BuilderType.OPEN_BRACKET;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.BuilderExpression;
import building.types.specific.data.ExpectedType;
import interpreting.program.ProgramLine;

/**
 * This is the Super-Interface for all Types. It gets used in every {@link Expression}.
 */
public interface AbstractType {

	/**
	 * Use this when any kind of {@link ValueHolder} is expected.
	 * 
	 * [LITERAL, NAME, ARRAY_START, OPEN_BRACKET]
	 */
	public static AbstractType[] valHolderTypes() {
		return new AbstractType[] { ARRAY_START, OPEN_BRACKET, EXPECTED_TYPE, EXPRESSION_TYPE };
	}

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

	/**
	 * Checks, if the passed {@link String} matches this type.
	 * 
	 * Gets called in {@link ProgramLine}.
	 * 
	 * Should get overridden by all none-specific Types like {@link SuperType} and {@link ExpectedType}.
	 */
	default boolean is(String arg) {
		return toString().equals(arg);
	}
}
