package types.specific.data;

import static types.specific.BuilderType.CLOSE_BRACKET;
import static types.specific.ExpressionType.NAME;
import static types.specific.ExpressionType.OPEN_SCOPE;

import java.util.stream.Stream;

import types.AbstractType;

/**
 * Super-Interface for {@link DataType} and {@link ArrayType}.
 */
public interface ExpectedType extends AbstractType {

	/** Returns an array of following expected expressions. */
	static AbstractType[] getExpected() {
		return new AbstractType[] { NAME, OPEN_SCOPE, CLOSE_BRACKET };
	}

	/** Returns an array of all {@link DataType}s and {@link ArrayType}s. */
	static ExpectedType[] values() {
		return Stream.of(DataType.values(), ArrayType.values()).flatMap(Stream::of).toArray(ExpectedType[]::new);
	}
}
