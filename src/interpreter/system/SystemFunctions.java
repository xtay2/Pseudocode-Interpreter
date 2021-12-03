package interpreter.system;

import java.util.Arrays;

import expressions.special.Type;
import expressions.special.Value;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.Interpreter;

public final class SystemFunctions {

	/** Enum with all Systemfunctions and their names. */
	public static enum SYSTEM_FUNCTION {

		PRINT("print"), EXIT("exit"), EXECUTE("execute"), ROUND("rnd");

		public final String name;

		private SYSTEM_FUNCTION(String name) {
			this.name = name;
		}
	}

	/** Returns if the specified name matches the name of a system-function. */
	public static SYSTEM_FUNCTION isSystemFunction(String name) {
		for (SYSTEM_FUNCTION f : SYSTEM_FUNCTION.values())
			if (f.name.equals(name))
				return f;
		return null;
	}

	public static Value callSystemFunc(SYSTEM_FUNCTION func, ValueHolder... params) {
		return switch (func) {
		case PRINT -> print(params);
		case EXIT -> exit(params);
		case EXECUTE -> execute(params);
		case ROUND -> round(params);
		default -> throw new NullPointerException();
		};
	}

	private static Value print(ValueHolder[] params) {
		if (params.length != 1)
			throw new IllegalArgumentException("Print takes only one value. Has " + Arrays.toString(params));
		Value val = params[0].getValue();
		System.out.println((Output.DEBUG ? "Printing: " : "") + (val.getType() == Type.BOOL ? val.asBool() : val.asText()));
		return null;
	}

	private static Value exit(ValueHolder[] params) {
		String exitMsg = "Exit";
		if (params.length == 1)
			exitMsg = params[0].getValue().asText();
		System.err.println(exitMsg);
		System.exit(0);
		return null;
	}

	private static Value execute(ValueHolder[] params) {
		if (params.length > 0) {
			String funcName = params[0].getValue().asText();
			ValueHolder[] funcParams = new ValueHolder[params.length - 1];
			System.arraycopy(params, 1, funcParams, 0, params.length - 1);
			return Interpreter.call(funcName, true, funcParams);
		}
		throw new IllegalArgumentException("The execute-function needs the name of the function that should get executed.");
	}

	private static Value round(ValueHolder[] params) {
		if (params.length < 1 || params.length > 2)
			throw new IllegalArgumentException("Round takes only one or two value. Has " + Arrays.toString(params));
		double val = params[0].getValue().asDouble();
		int comma = 0;
		if (params.length == 2)
			comma = params[1].getValue().asInt();
		return new Value((double) Math.round(val * Math.pow(10, comma)) / Math.pow(10, comma), Type.NUMBER);
	}
}
