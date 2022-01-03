package expressions.main;

import static helper.Output.print;

import java.util.ArrayList;
import java.util.Arrays;

import datatypes.Value;
import expressions.special.Expression;
import expressions.special.MainExpression;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.Interpreter;

public class Call extends MainExpression implements ValueHolder {

	private String calledFunc = null;
	private ValueHolder[] parameters = null;

	public Call(int line) {
		super(line);
	}

	/** Call wird seperat initialisiert. Siehe: Call.init() */
	@Override
	public void build(Expression... args) {

	}

	/** Gets called when lines merge themselves */
	public void init(String name, ArrayList<ValueHolder> params) {
		calledFunc = name;
		parameters = new ValueHolder[params == null ? 0 : params.size()];
		for (int i = 0; i < parameters.length; i++)
			parameters[i] = params.get(i);
	}

	public String getFuncName() {
		return calledFunc;
	}

	@Override
	public Value getValue() {
		if (parameters == null)
			return Interpreter.call(calledFunc, true);
		return Interpreter.call(calledFunc, true, parameters);
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Calling Function " + calledFunc + (parameters.length == 0 ? "" : " with " + Arrays.toString(parameters)));
		getValue();
		return callNextLine(doExecuteNext);
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "call " + getFuncName() + (parameters.length != 0 ? ": " + Arrays.toString(parameters) : "");
	}
}
