package types;

import java.lang.reflect.Method;

import expressions.abstractions.Expression;
import modules.finder.ExpressionFinder;
import types.specific.BuilderType;
import types.specific.DataType;
import types.specific.ExpressionType;
import types.specific.FlagType;
import types.specific.KeywordType;

public enum SuperType implements AbstractType {
	BUILDER_TYPE(BuilderType.class), DATA_TYPE(DataType.class), EXPRESSION_TYPE(ExpressionType.class), FLAG_TYPE(FlagType.class),
	KEYWORD_TYPE(KeywordType.class);

	private final Class<?> classType;

	SuperType(Class<?> classType) {
		this.classType = classType;
	}

	@Override
	public Expression create(String arg, int lineID) {
		try {
			// Invoke static method "values" in enums.
			Method values = classType.getMethod("values");
			return ExpressionFinder.find(arg, (AbstractType[]) values.invoke(null), lineID);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public boolean is(SuperType superType) {
		return false;
	}
}
