package building.expressions.normal.operators;

import java.util.*;

import org.junit.jupiter.api.*;

import building.expressions.abstractions.interfaces.*;
import building.expressions.normal.operators.infix.*;
import building.expressions.normal.operators.postfix.*;
import building.types.specific.operators.*;
import runtime.datatypes.numerical.*;

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
