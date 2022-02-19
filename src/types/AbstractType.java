package types;

import expressions.abstractions.Expression;
import modules.finder.ExpressionFinder;
import modules.parser.program.ProgramLine;
import types.specific.BuilderType;
import types.specific.DataType;
import types.specific.ExpressionType;
import types.specific.FlagType;
import types.specific.KeywordType;

/**
 * This is a marker Interface for all Types. It gets used in every {@link Expression}.
 * 
 * @see BuilderType
 * @see DataType
 * @see KeywordType
 * @see ExpressionType
 * @see FlagType
 */
public interface AbstractType {

	/**
	 * Used to create an Expression from this {@link AbstractType} and a {@link String}.
	 * 
	 * This gets called in the {@link ExpressionFinder}.
	 * 
	 * @param arg    is the String.
	 * @param lineID is the line-identifier of the calling {@link ProgramLine}.
	 * @return null if the {@link String} didn't match this type.
	 * @return the {@link Expression} or {@code null} if the {@link String} doesn't match.
	 */
	Expression create(String arg, int lineID);

	/** Checks if this Type is the Subtype of the given {@link SuperType}. */
	boolean is(SuperType superType);
}
