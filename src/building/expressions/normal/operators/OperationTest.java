package building.expressions.normal.operators;

import java.util.List;

import org.junit.jupiter.api.Test;

import building.expressions.abstractions.interfaces.Operatable;
import building.expressions.normal.operators.infix.ArithmeticOperator;
import building.expressions.normal.operators.infix.InfixOperator;
import building.expressions.normal.operators.postfix.PostfixOperator;
import building.types.specific.operators.InfixOpType;
import building.types.specific.operators.PostfixOpType;
import runtime.datatypes.numerical.IntValue;

public class OperationTest {

	@Test
	void testOperation() {
		IntValue i = new IntValue(2), j = new IntValue(3);
		PostfixOperator inc = new PostfixOperator(-1, PostfixOpType.INC, i);
		InfixOperator p = new ArithmeticOperator(-1, InfixOpType.ADD);
		InfixOperator m = new ArithmeticOperator(-1, InfixOpType.MULT);
		assert create(i, p, j).getValue().equals(5);
		assert create(i, p, i, m, j).getValue().equals(8);
		assert create(inc, m, i).equals(4);
	}

	Operation create(Operatable... operatables) {
		return new Operation(-1, List.of(operatables));
	}

}
