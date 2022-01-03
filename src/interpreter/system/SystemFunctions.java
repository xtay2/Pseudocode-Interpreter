package interpreter.system;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;

import datatypes.Value;
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
	public static SYSTEM_FUNCTION isSystemFunction(String name) {
		for (SYSTEM_FUNCTION f : SYSTEM_FUNCTION.values())
			if (f.name.equals(name))
				return f;
		return null;
	}

	public static Value callSystemFunc(SYSTEM_FUNCTION func, ValueHolder... params) {
		return switch (func) {
		case PRINT -> print(params);
		case READ -> read(params);
		case EXIT -> exit(params);
		case EXECUTE -> execute(params);
		case TYPE -> type(params);
		default -> throw new NullPointerException();
		};
	}

	private static Value print(ValueHolder[] params) {
		if (params.length != 1)
			throw new IllegalArgumentException("Print takes only one value. Has " + Arrays.toString(params));
		Value val = params[0].getValue();
		System.out.println((Output.DEBUG ? "Printing: " : "") + (val.asText().rawString()));
		return null;
	}

	private static Value read(ValueHolder[] params) {
		if (params.length != 1)
			throw new IllegalArgumentException("Read takes only one value. Has " + Arrays.toString(params));
		System.out.println(params[0].getValue().asText().rawString());
		BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
		try {
			return new TextValue(reader.readLine());
		} catch (IOException e) {
			return null;
		}
	}

	private static Value exit(ValueHolder[] params) {
		Value exitMsg = new TextValue("Exit");
		if (params.length == 1)
			exitMsg = params[0].getValue().asText();
		System.err.println(exitMsg);
		System.exit(0);
		return null;
	}

	private static Value execute(ValueHolder[] params) {
		if (params.length > 0) {
			String funcName = params[0].getValue().asText().rawString();
			ValueHolder[] funcParams = new ValueHolder[params.length - 1];
			System.arraycopy(params, 1, funcParams, 0, params.length - 1);
			return Interpreter.call(funcName, true, funcParams);
		}
		throw new IllegalArgumentException("The execute-function needs the name of the function that should get executed.");
	}

	private static Value type(ValueHolder[] params) {
		if (params.length != 1)
			throw new IllegalArgumentException("Type takes just one parameter.\nGot " + Arrays.toString(params));
		return new TextValue(params[0].getValue().getType().toString());
	}
}
