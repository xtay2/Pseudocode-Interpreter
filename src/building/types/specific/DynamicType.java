package building.types.specific;

import static building.types.abstractions.SuperType.AFTER_VALUE_TYPE;
import static building.types.abstractions.SuperType.ASSIGNMENT_TYPE;
import static building.types.specific.BuilderType.ARRAY_START;
import static building.types.specific.BuilderType.OPEN_BRACKET;
import static building.types.specific.BuilderType.RANGE;

import building.expressions.normal.BuilderExpression;
import building.expressions.normal.containers.Name;
import building.types.abstractions.AbstractType;
import building.types.abstractions.SpecificType;
import interpreting.program.ValueBuilder;

/**
 * Every type that the user can dynamically specify.
 */
public enum DynamicType implements SpecificType {

	LITERAL, NAME;

	@Override
	public BuilderExpression create(String arg, int lineID) {
		if (!switch (this) {
			case LITERAL -> ValueBuilder.isLiteral(arg);
			case NAME -> Name.isName(arg);
		}) {
			return null;
		}
		// Calls the specialised Constructor.
		return new BuilderExpression(lineID, this, arg);
	}

	@Override
	public AbstractType[] abstractExpected() {
		return switch (this) {
			case NAME -> new AbstractType[] { AFTER_VALUE_TYPE, ASSIGNMENT_TYPE, OPEN_BRACKET, ARRAY_START };
			case LITERAL -> new AbstractType[] { AFTER_VALUE_TYPE, RANGE };
		};
	}

	@Override
	public String toString() {
		return name().toLowerCase();
	}
}
