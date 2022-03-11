package building.types.specific.data;

import static building.types.specific.BuilderType.CLOSE_BRACKET;
import static building.types.specific.BuilderType.COMMA;
import static building.types.specific.BuilderType.OPEN_BLOCK;
import static building.types.specific.ExpressionType.NAME;

import java.util.stream.Stream;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.AbstractType;

/**
 * Super-Interface for {@link DataType} and {@link ArrayType}.
 */
public interface ExpectedType extends AbstractType {

	/** Returns an array of all {@link DataType}s and {@link ArrayType}s. */
	static ExpectedType[] values() { // NO_UCD
		return Stream.of(DataType.values(), ArrayType.values()).flatMap(Stream::of).toArray(ExpectedType[]::new);
	}

	@Override
	public default AbstractType[] expected() {
		return new AbstractType[] { NAME, OPEN_BLOCK, CLOSE_BRACKET, COMMA };
	}

	/**
	 * Returns the standard-value for this type.
	 * 
	 * It gets in an empty declaration like: int b
	 */
	ValueHolder stdVal();
}
