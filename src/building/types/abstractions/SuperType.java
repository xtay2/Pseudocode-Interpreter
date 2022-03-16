package building.types.abstractions;

import static building.types.specific.BuilderType.*;

import building.types.specific.AssignmentType;
import building.types.specific.BuilderType;
import building.types.specific.DataType;
import building.types.specific.DynamicType;
import building.types.specific.FlagType;
import building.types.specific.KeywordType;
import building.types.specific.operators.InfixOpType;
import building.types.specific.operators.PostfixOpType;
import building.types.specific.operators.PrefixOpType;

public enum SuperType implements UnspecificType {

	// Specific Types
	DYNAMIC_TYPE(DynamicType.values()),

	FLAG_TYPE(FlagType.values()),

	KEYWORD_TYPE(KeywordType.values()),

	BUILDER_TYPE(BuilderType.values()),

	PREFIX_OP_TYPE(PrefixOpType.values()),

	INFIX_OP_TYPE(InfixOpType.values()),

	POSTFIX_OP_TYPE(PostfixOpType.values()),

	ASSIGNMENT_TYPE(AssignmentType.values()),

	DATA_TYPE(DataType.values()),

	// Groups
	ARRAY_TYPE(DataType.arrayValues()),

	VAL_HOLDER_TYPE(ARRAY_START, OPEN_BRACKET, DATA_TYPE, DYNAMIC_TYPE, PREFIX_OP_TYPE),

	AFTER_VALUE_TYPE(INFIX_OP_TYPE, KEYWORD_TYPE, COMMA, CLOSE_BRACKET, ARRAY_END, OPEN_BLOCK),

	START_OF_LINE_TYPE(KEYWORD_TYPE, VAL_HOLDER_TYPE, FLAG_TYPE, CLOSE_BLOCK, MULTI_CALL_LINE);

	private final AbstractType[] subValues;

	SuperType(AbstractType... subValues) {
		this.subValues = subValues;
	}

	@Override
	public AbstractType[] directSubValues() {
		return subValues;
	}

}
