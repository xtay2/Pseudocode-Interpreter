package interpreter.system;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import datatypes.NumberValue;
import datatypes.TextValue;
import datatypes.Value;
import expressions.special.ValueHolder;
import helper.Output;

public final class SystemFunctions {

	/** Enum with all Systemfunctions and their names. */
	public static enum SYSTEM_FUNCTION {

		EXIT("exit"), PRINT("print"), READ("read"), TYPE("type"), AS_RATIONAL("asRational");

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
		case TYPE -> type(params);
		case AS_RATIONAL -> asRational(params);
		};
	}

	/** Returns the matching system-function for this name. */
	public static SYSTEM_FUNCTION getSystemFunction(String name) {
		for (SYSTEM_FUNCTION f : SYSTEM_FUNCTION.values())
			if (f.name.equals(name))
				return f;
		return null;
	}

	/** native func exit(text msg) */
	private static Value exit(ValueHolder[] params) {
		String exitMsg = params[0].getValue().asText().rawString();
		System.err.println(exitMsg);
		System.exit(0);
		return null;
	}

	/** native func read(text msg) */
	private static Value print(ValueHolder[] params) {
		System.out.println((Output.DEBUG ? "Printing: " : "") + (params[0].getValue().asText().rawString()));
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

	/** Implementation: native func asRational(nr n) -> text */
	private static Value asRational(ValueHolder[] params) {
		NumberValue n = params[0].getValue().asNumber();
		return new TextValue(n.asRational());
	}

	private static Value type(ValueHolder[] params) {
		return new TextValue(params[0].getValue().getType().toString());
	}
}
