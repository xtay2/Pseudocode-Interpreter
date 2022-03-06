package misc.tests.datatypes;

import static runtime.datatypes.numerical.ConceptualNrValue.*;
import static runtime.datatypes.numerical.NumberValue.NEG_ONE;
import static runtime.datatypes.numerical.NumberValue.ONE;
import static runtime.datatypes.numerical.NumberValue.ZERO;
import static runtime.datatypes.numerical.NumberValue.create;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

import org.junit.jupiter.api.Test;

import runtime.datatypes.TextValue;
import runtime.datatypes.numerical.NumberValue;

public class Number {

	interface ArithmeticOperation {
		NumberValue execute(NumberValue a, NumberValue b);
	}

	@Test
	void testOperations() {
		List<NumberValue> l = List.of(
		//@formatter:off
			ZERO, 
			ONE, 
			NEG_ONE, 
			create(BigDecimal.valueOf(0.5)),
			create(BigDecimal.valueOf(-0.5)), 
			create(BigInteger.valueOf(100)),
			create(BigInteger.valueOf(-100)), 
			create(BigInteger.valueOf(7)),
			create(BigInteger.valueOf(-7)), 
			POS_INF, 
			NEG_INF, 
			NAN,
			create(BigInteger.valueOf(1), BigInteger.valueOf(3)),
			create(BigInteger.valueOf(-1), BigInteger.valueOf(3)),
			new TextValue("-10.745(3701)").asNumber()
		//@formatter:on
		);
		testPerms((a, b) -> a.add(b), l, "+");
		testPerms((a, b) -> a.sub(b), l, "-");
		testPerms((a, b) -> a.mult(b), l, "*");
		testPerms((a, b) -> a.div(b), l, "/");
		testPerms((a, b) -> a.mod(b), l, "%");
		testPerms((a, b) -> a.pow(b), l, "^");
		testPerms((a, b) -> a.root(b), l, "root");
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
