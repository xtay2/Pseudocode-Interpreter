package interpreter.system;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import datatypes.Value;
import exceptions.runtime.IllegalCallException;
import datatypes.ArrayValue;
import datatypes.TextValue;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.Interpreter;

public final class SystemFunctions {

	/** Enum with all Systemfunctions and their names. */
	public static enum SYSTEM_FUNCTION {

		PRINT("print"), READ("read"), EXIT("exit"), EXECUTE("execute"), TYPE("type");

		public final String name;

		private SYSTEM_FUNCTION(String name) {
			this.name = name;
		}
	}

	/** Returns if the specified name matches the name of a system-function. */
	private static SYSTEM_FUNCTION isSystemFunction(String name) {
		for (SYSTEM_FUNCTION f : SYSTEM_FUNCTION.values())
			if (f.name.equals(name))
				return f;
		return null;
	}

	public static Value callSystemFunc(String func, ValueHolder... params) {
		return switch (isSystemFunction(func)) {
		case PRINT -> print(params);
		case READ -> read(params);
		case EXIT -> exit(params);
		case EXECUTE -> execute(params);
		case TYPE -> type(params);
		default -> throw new IllegalCallException("Only functions from the standard library can use the native keyword.");
		};
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

	private static Value exit(ValueHolder[] params) {
		String exitMsg = params[0].getValue().toString();
		System.err.println(exitMsg);
		System.exit(0);
		return null;
	}

	private static Value execute(ValueHolder[] params) {
		String funcName = params[0].getValue().asText().rawString();
		ArrayValue arr = ((ArrayValue) params[1].getValue());
		return Interpreter.call(funcName, true, arr.rawArray());
	}

	private static Value type(ValueHolder[] params) {
		return new TextValue(params[0].getValue().getType().toString());
	}
}
