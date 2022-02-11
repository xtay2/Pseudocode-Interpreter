package unittests.datatypes;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

import org.junit.jupiter.api.Test;

import datatypes.NumberValue;
import datatypes.TextValue;

public class Number {

	interface ArithmeticOperation {
		NumberValue execute(NumberValue a, NumberValue b);
	}

	@Test
	void testOperations() {
		List<NumberValue> l = List.of(
		//@formatter:off
			NumberValue.ZERO, 
			NumberValue.ONE, 
			NumberValue.NEG_ONE, 
			NumberValue.create(BigDecimal.valueOf(0.5)),
			NumberValue.create(BigDecimal.valueOf(-0.5)), 
			NumberValue.create(BigInteger.valueOf(100)),
			NumberValue.create(BigInteger.valueOf(-100)), 
			NumberValue.create(BigInteger.valueOf(7)),
			NumberValue.create(BigInteger.valueOf(-7)), 
			NumberValue.POS_INF, 
			NumberValue.NEG_INF, 
			NumberValue.NAN,
			NumberValue.create(BigInteger.valueOf(1), BigInteger.valueOf(3)),
			NumberValue.create(BigInteger.valueOf(-1), BigInteger.valueOf(3)),
			new TextValue("-10.745(3701)").asNumber()
		//@formatter:on
		);
//		testPerms((a, b) -> NumberValue.add(a, b), l, "+");
//		testPerms((a, b) -> NumberValue.sub(a, b), l, "-");
//		testPerms((a, b) -> NumberValue.mult(a, b), l, "*");
//		testPerms((a, b) -> NumberValue.div(a, b), l, "/");
//		testPerms((a, b) -> NumberValue.mod(a, b), l, "%");
		testPerms((a, b) -> NumberValue.pow(a, b), l, "^");
		testPerms((a, b) -> NumberValue.root(a, b), l, "root");
	}

	void testPerms(ArithmeticOperation o, List<NumberValue> l, String op) {
		for (NumberValue a : l) {
			for (NumberValue b : l) {
				long s = System.currentTimeMillis();
				long e;
				try {
					NumberValue res = o.execute(a, b);
					e = System.currentTimeMillis() - s;
					System.out.println(time(e) + a + " " + op + " " + b + " = " + res);
				} catch (ArithmeticException aE) {
					e = System.currentTimeMillis() - s;
					System.out.println(time(e) + a + " " + op + " " + b + " = " + aE.getLocalizedMessage());
				}
			}
		}
	}

	String time(long t) {
		String u = "ms";
		if (t > 1000) {
			t /= 1000;
			u = "sek";
			if (t > 60) {
				t /= 60;
				u = "min+";
			}
		}
		return "[" + t + " " + u + "] ";
	}
}
