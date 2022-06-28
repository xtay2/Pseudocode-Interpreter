package misc.tests.value;

import static building.expressions.abstractions.Range.*;
import static building.types.specific.datatypes.SingleType.*;

import java.math.*;
import java.util.*;

import org.junit.jupiter.api.*;

import building.expressions.abstractions.*;
import building.types.specific.datatypes.*;
import errorhandeling.*;
import runtime.datatypes.*;
import runtime.datatypes.array.*;
import runtime.datatypes.numerical.*;
import runtime.datatypes.textual.*;

public class ValueTests {
	
	@Test
	void testMatches() throws NonExpressionException {
		// Int
		IntValue i = new IntValue(0);
		testMatch(i, false, VAR, INT, NR);
		
		// Nr
		NumberValue nr = NumberValue.create(BigDecimal.valueOf(0.5));
		testMatch(nr, false, VAR, NR);
		
		// Text
		TextValue txt = new TextValue("Hello");
		testMatch(txt, false, VAR, TEXT);
		
		// Char
		CharValue c = new CharValue('A');
		testMatch(c, false, VAR, CHAR, TEXT);
		
		// Bool
		BoolValue b = BoolValue.TRUE;
		testMatch(b, false, VAR, BOOL);
		
		// Null
		Value n = MaybeValue.NULL;
		testMatch(n, true, SingleType.values());
		testMatch(n, false);
		
		// Arrays
		ArrayValue arr0 = new ArrayValue(new DataType(INT, false, UNBOUNDED), i, i, i);
		testMatch(arr0, false, VAR);
		
		// Type
		ArrayValue arr1 = new ArrayValue(new DataType(VAR, true, UNBOUNDED), txt);
		assert arr1.allowsLosslessCastingTo(new DataType(VAR, true, UNBOUNDED)); // ["Hello"] -> var?[]
		assert arr1.allowsLosslessCastingTo(new DataType(TEXT, true, UNBOUNDED)); // ["Hello"] -> text?[]
		assert !arr1.allowsLosslessCastingTo(new DataType(BOOL, true, UNBOUNDED)); // ["Hello"] -> bool?[]
		
		// Null
		ArrayValue arr2 = new ArrayValue(new DataType(VAR, true, Range.UNBOUNDED), n);
		assert arr2.allowsLosslessCastingTo(new DataType(VAR, true, Range.exact(1))); // [null] -> var?[1]
		assert !arr2.allowsLosslessCastingTo(new DataType(VAR, false, Range.exact(1))); // [null] -> var[1]
		
		// Length
		ArrayValue arr3 = new ArrayValue(new DataType(BOOL, true, upperBound(3)), b, b);
		assert arr3.allowsLosslessCastingTo(new DataType(BOOL, false, upperBound(2))); // [true, true] -> bool[..2]
		assert arr3.allowsLosslessCastingTo(new DataType(BOOL, false, lowerBound(2))); // [true, true] -> bool[2..]
		assert !arr3.allowsLosslessCastingTo(new DataType(BOOL, false, exact(1))); // [true] -> bool[1]
		
		// Multidimensional
		ArrayValue arr4 = ArrayValue.newInstance(arr1, arr2, arr3);
		assert arr4.allowsLosslessCastingTo(new DataType(VAR, true, Range.exact(3))); // [["Hello"], [null], [true, true]] -> var?[]
		assert !arr4.allowsLosslessCastingTo(new DataType(VAR, false, UNBOUNDED)); // [["Hello"], [null], [true, true]] -> var[]
		assert !arr4.allowsLosslessCastingTo(new DataType(TEXT, true, UNBOUNDED)); // [["Hello"], [null], [true, true]] -> var[]
		
		ArrayValue arr5 = ArrayValue.newInstance(ArrayValue.newInstance());
		assert arr5.allowsLosslessCastingTo(new DataType(VAR, true, UNBOUNDED)); // [[]] -> var?[]
		assert arr5.allowsLosslessCastingTo(new DataType(VAR, true, UNBOUNDED, UNBOUNDED)); // [[]] -> var?[][]
		assert !arr5.allowsLosslessCastingTo(new DataType(VAR, true, UNBOUNDED, UNBOUNDED, UNBOUNDED)); // [[]] -> var?[][][]
		
		ArrayValue arr6 = ArrayValue.newInstance(ArrayValue.newInstance(), n);
		assert arr6.allowsLosslessCastingTo(new DataType(VAR, true, UNBOUNDED)); // [[], null] -> var?[]
		assert !arr6.allowsLosslessCastingTo(new DataType(VAR, true, UNBOUNDED, UNBOUNDED)); // [[], null] -> var?[][]
	}
	
	/**
	 * Tests if a value matches certain types.
	 *
	 * @param v can be any value
	 * @param allowsNull == false causes an {@link AssertionError} for the {@link MaybeValue#NULL}
	 * @param allowedTypes are the types that assert to be allowed
	 */
	void testMatch(Value v, boolean allowsNull, SingleType... allowedTypes) {
		for (SingleType t : SingleType.values()) {
			if (Arrays.stream(allowedTypes).anyMatch(e -> e == t))
				assert v.matches(new DataType(t, allowsNull));
			else
				assert !v.matches(new DataType(t, allowsNull));
		}
	}
}
