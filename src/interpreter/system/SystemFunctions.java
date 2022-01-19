package interpreter.system;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import datatypes.ArrayValue;
import datatypes.TextValue;
import datatypes.Value;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.Interpreter;

public final class SystemFunctions {

	/** Enum with all Systemfunctions and their names. */
	public static enum SYSTEM_FUNCTION {

		EXECUTE("execute"), EXIT("exit"), PRINT("print"), READ("read"), TYPE("type");

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
		case EXECUTE -> execute(params);
		case TYPE -> type(params);
		};
	}

	private static Value execute(ValueHolder[] params) {
		String funcName = params[0].getValue().asText().rawString();
		ArrayValue arr = ((ArrayValue) params[1].getValue());
		return Interpreter.call(funcName, true, arr.raw(true));
	}

	private static Value exit(ValueHolder[] params) {
		String exitMsg = params[0].getValue().toString();
		System.err.println(exitMsg);
		System.exit(0);
		return null;
	}

	/** Returns the matching system-function for this name. */
	public static SYSTEM_FUNCTION getSystemFunction(String name) {
		for (SYSTEM_FUNCTION f : SYSTEM_FUNCTION.values())
			if (f.name.equals(name))
				return f;
		return null;
	}

	private static Value print(ValueHolder[] params) {
		System.out.println((Output.DEBUG ? "Printing: " : "") + (params[0].getValue().asText().rawString()));
		return null;
	}

	private static Value read(ValueHolder[] params) {
		BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
		try {
			return new TextValue(reader.readLine());
		} catch (IOException e) {
			return null;
		}
	}

	private static Value type(ValueHolder[] params) {
		return new TextValue(params[0].getValue().getType().toString());
	}
}
