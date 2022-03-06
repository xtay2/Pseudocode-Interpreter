package runtime.natives;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.util.Random;

import building.expressions.abstractions.interfaces.ValueHolder;
import misc.helper.Output;
import runtime.datatypes.TextValue;
import runtime.datatypes.Value;
import runtime.datatypes.numerical.DecimalValue;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;

public final class SystemFunctions {

	/** Enum with all Systemfunctions and their names. */
	public static enum SYSTEM_FUNCTION {

		/** Terminates the program. */
		EXIT("exit"),

		/** Prints something in the console. */
		PRINT("print"),

		/** Reads something from the console. */
		READ("read"),

		/** Returns a fractional representation of a number. */
		AS_RATIONAL("asRational"),

		/** Returns a random {@link NumberValue} in a specified range. */
		RAND_NR("randNr"),

		/** Returns a random {@link IntValue} in a specified range. */
		RAND_INT("randInt");

		public final String name;

		private SYSTEM_FUNCTION(String name) {
			this.name = name;
		}
	}

	public static Value callSystemFunc(SYSTEM_FUNCTION func, ValueHolder... params) {
		return switch (func) {
			case PRINT -> print(params);
			case READ -> read(params);
			case EXIT -> exit(params);
			case AS_RATIONAL -> asRational(params);
			case RAND_NR -> randNr(params);
			case RAND_INT -> randInt(params);
		};
	}

	/** Returns the matching system-function for this name. */
	public static SYSTEM_FUNCTION getSystemFunction(String name) {
		for (SYSTEM_FUNCTION f : SYSTEM_FUNCTION.values())
			if (f.name.equals(name))
				return f;
		return null;
	}

	/** native func exit(text) */
	private static Value exit(ValueHolder[] params) {
		String exitMsg = params[0].getValue().asText().value;
		System.err.println(exitMsg);
		System.exit(0);
		return null;
	}

	/** native func read(text) */
	private static Value print(ValueHolder[] params) {
		System.out.println((Output.DEBUG ? "Printing: " : "") + (params[0].getValue().asText().value));
		return null;
	}

	/** native func read() -> text */
	private static Value read(ValueHolder[] params) {
		BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
		try {
			return new TextValue(reader.readLine());
		} catch (IOException e) {
			return null;
		}
	}

	/** Implementation: native func asRational(nr) -> text */
	private static Value asRational(ValueHolder[] params) {
		NumberValue n = params[0].getValue().asNumber();
		if (n instanceof DecimalValue d)
			return new TextValue(d.asRational());
		return n instanceof IntValue i ? new TextValue(i.value.toString() + "/1") : n.asText();
	}

	/** Implementation: native func randNr(nr, nr) -> nr */
	private static NumberValue randNr(ValueHolder[] params) {
		throw new AssertionError("This feature isn't implemented yet.");
	}

	/** Implementation: native func randInt(int, int) -> int */
	private static IntValue randInt(ValueHolder[] params) {
		IntValue min = params[0].getValue().asInt();
		IntValue max = params[1].getValue().asInt().add(NumberValue.ONE).asInt();
		if (min.isGreaterThan(max))
			throw new IllegalArgumentException("Then minimum cannot be greater than the maximum.");
		BigInteger upperLimit = max.sub(min).asInt().raw();
		BigInteger randomNumber;
		do {
			randomNumber = new BigInteger(upperLimit.bitLength(), new Random());
		} while (randomNumber.compareTo(upperLimit) >= 0);
		return min.add(NumberValue.create(randomNumber)).asInt();
	}
}
