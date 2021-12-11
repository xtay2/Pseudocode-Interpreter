package interpreter.system;

import java.util.Arrays;

import datatypes.Castable;
import datatypes.NumberValue;
import datatypes.TextValue;
import expressions.special.Type;
import expressions.special.Value;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.Interpreter;

public final class SystemFunctions {

	/** Enum with all Systemfunctions and their names. */
	public static enum SYSTEM_FUNCTION {

		PRINT("print"), EXIT("exit"), EXECUTE("execute"), TYPE("type");

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

	public static Castable callSystemFunc(SYSTEM_FUNCTION func, ValueHolder... params) {
		return switch (func) {
		case PRINT -> print(params);
		case EXIT -> exit(params);
		case EXECUTE -> execute(params);
		case TYPE -> type(params);
		default -> throw new NullPointerException();
		};
	}

	private static Castable print(ValueHolder[] params) {
		if (params.length != 1)
			throw new IllegalArgumentException("Print takes only one value. Has " + Arrays.toString(params));
		Castable val = params[0].getValue();
		System.out.println((Output.DEBUG ? "Printing: " : "") + (val.toString()));
		return null;
	}

	private static Castable exit(ValueHolder[] params) {
		Castable exitMsg = new TextValue("Exit");
		if (params.length == 1)
			exitMsg = params[0].getValue().asText();
		System.err.println(exitMsg);
		System.exit(0);
		return null;
	}

	private static Castable execute(ValueHolder[] params) {
		if (params.length > 0) {
			String funcName = params[0].getValue().asText().rawString();
			ValueHolder[] funcParams = new ValueHolder[params.length - 1];
			System.arraycopy(params, 1, funcParams, 0, params.length - 1);
			return Interpreter.call(funcName, true, funcParams);
		}
		throw new IllegalArgumentException("The execute-function needs the name of the function that should get executed.");
	}

	private static Castable type(ValueHolder[] params) {
		if (params.length != 1)
			throw new IllegalArgumentException("Type takes just one parameter.");
		return new TextValue(params[0].getValue().getType().toString());
	}
}
