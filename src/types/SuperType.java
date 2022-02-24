package types;

import java.lang.reflect.Method;

import expressions.abstractions.Expression;
import modules.finder.ExpressionFinder;
import types.specific.BuilderType;
import types.specific.DataType;
import types.specific.ExpressionType;
import types.specific.FlagType;
import types.specific.KeywordType;
import types.specific.operators.InfixOpType;
import types.specific.operators.PostfixOpType;
import types.specific.operators.PrefixOpType;

public enum SuperType implements AbstractType {

	BUILDER_TYPE(BuilderType.class), DATA_TYPE(DataType.class), EXPRESSION_TYPE(ExpressionType.class), FLAG_TYPE(FlagType.class),
	KEYWORD_TYPE(KeywordType.class),

	INFIX_OPERATOR(InfixOpType.class), PREFIX_OPERATOR(PrefixOpType.class), POSTFIX_OPERATOR(PostfixOpType.class),

	MERGED(null);

	private final Class<?> classType;

	SuperType(Class<?> classType) {
		this.classType = classType;
	}

	@Override
	public Expression create(String arg, int lineID) {
		if (classType == null)
			throw new AssertionError("Merged Expressions should get created in the value-Merger.");
		try {
			// Invoke static method "values" in enums.
			Method values = classType.getMethod("values");
			return ExpressionFinder.find(arg, lineID, (AbstractType[]) values.invoke(null));
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
