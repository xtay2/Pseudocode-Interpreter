package types;

import types.specific.BuilderType;
import types.specific.DataType;
import types.specific.FlagType;
import types.specific.KeywordType;

/**
 * Interface that groups all specific subtypes of {@link ExpressionType}, like:
 * 
 * <pre>
 * - {@link DataType}
 * - {@link FlagType}
 * - {@link KeywordType}
 * - {@link ReturnType}
 * </pre>
 */
public interface SpecificType extends AbstractType {

	/** Returns the superior ExpressionType. */
	default ExpressionType getExpressionType() {
		return switch (this) {
		case DataType d -> ExpressionType.DATA_TYPE;
		case FlagType f -> ExpressionType.FLAG;
		case KeywordType k -> ExpressionType.KEYWORD;
		case BuilderType b -> ExpressionType.BUILDER_TYPE;
		default -> throw new AssertionError(this + " has no specified or connected ExpressionType.");
		};
	}
}
