package building.types;

import java.lang.reflect.Method;
import java.util.Arrays;

import building.expressions.normal.BuilderExpression;
import building.expressions.normal.containers.ArrayAccess;
import building.expressions.normal.containers.Name;
import building.types.specific.AssignmentType;
import building.types.specific.BuilderType;
import building.types.specific.ExpressionType;
import building.types.specific.FlagType;
import building.types.specific.KeywordType;
import building.types.specific.data.ArrayType;
import building.types.specific.data.DataType;
import building.types.specific.data.ExpectedType;
import building.types.specific.operators.InfixOpType;
import building.types.specific.operators.PostfixOpType;
import building.types.specific.operators.PrefixOpType;
import interpreting.program.StringConverter;

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
	ASSIGNMENT_TYPE(AssignmentType.class),

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

	private final Class<? extends AbstractType> classType;

	SuperType(Class<? extends AbstractType> classType) {
		this.classType = classType;
	}

	@Override
	public BuilderExpression create(String arg, int lineID) {
		if (this == MERGED)
			throw new AssertionError("Merged Expressions should get created in the value-Merger.");
		return StringConverter.find(arg, lineID, subValues());
	}

	@Override
	public boolean is(SuperType superType) {
		return this == superType;
	}

	@Override
	public boolean is(String arg) {
		return Arrays.stream(subValues()).anyMatch(e -> arg.equals(e.toString()));
	}

	private AbstractType[] subValues() {
		try {
			// Invoke static method "values" in enums.
			Method values = classType.getMethod("values");
			return (AbstractType[]) values.invoke(null);
		} catch (Exception e) {
			throw new AssertionError("Couldn't execute values() on " + this);
		}
	}

	@Override
	public AbstractType[] expected() {
		throw new AssertionError("This method should never get called, because SuperType is an abstract Type.");
	}
}
