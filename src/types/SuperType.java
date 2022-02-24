package types;

import java.lang.reflect.Method;

import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ScopeBracket;
import expressions.normal.containers.ArrayAccess;
import expressions.normal.containers.Name;
import modules.finder.ExpressionFinder;
import types.specific.AssingmentType;
import types.specific.BuilderType;
import types.specific.ExpressionType;
import types.specific.FlagType;
import types.specific.KeywordType;
import types.specific.data.ArrayType;
import types.specific.data.DataType;
import types.specific.data.ExpectedType;
import types.specific.operators.InfixOpType;
import types.specific.operators.PostfixOpType;
import types.specific.operators.PrefixOpType;

public enum SuperType implements AbstractType {

	/** Every {@link BuilderType}. For example: |, [, ). */
	BUILDER_TYPE(BuilderType.class),

	/** Every {@link FlagType}. For example: native, const. */
	FLAG_TYPE(FlagType.class),

	//////////////////////////////////////////////////////
	/** Both {@link DataType} and {@link ArrayType} */
	EXPECTED_TYPE(ExpectedType.class),

	/** Every {@link DataType}. For example: var, int. */
	DATA_TYPE(DataType.class),

	/** Every {@link ArrayType}. For example: var[], nr[]. */
	ARRAY_TYPE(ArrayType.class),
	//////////////////////////////////////////////////////

	/** Every {@link ExpressionType}. For example: {@link Name} and {@link ScopeBracket}. */
	EXPRESSION_TYPE(ExpressionType.class),

	/** Every {@link KeywordType}. For example: func, if, return */
	KEYWORD_TYPE(KeywordType.class),

	/** Every {@link AssingmentType}. For example: =, +=, *= */
	ASSIGNMENT_TYPE(AssingmentType.class),

	//////////////////////////////////////////////////////
	/** Every {@link InfixOpType}. For example: +, and, in */
	INFIX_OPERATOR(InfixOpType.class),

	/** Every {@link PrefixOpType}. For example: ++, --, not */
	PREFIX_OPERATOR(PrefixOpType.class),

	/** Every {@link PostfixOpType}. For example: ++, --, ! */
	POSTFIX_OPERATOR(PostfixOpType.class),
	//////////////////////////////////////////////////////

	/** Fully Merged Expressions, like {@link ArrayAccess}. */
	MERGED(null);

	//////////////////////////////////////////////////////

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
