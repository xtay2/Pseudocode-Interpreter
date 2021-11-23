package programreader.expressions.main;

import java.util.ArrayList;
import java.util.Arrays;

import programreader.expressions.special.Expression;
import programreader.expressions.special.MainExpression;
import programreader.expressions.special.Value;
import programreader.expressions.special.ValueHolder;
import programreader.interpreter.Interpreter;

import static helper.Output.*;

public class Call extends MainExpression implements ValueHolder {

	private String calledFunc = null;
	private ValueHolder[] parameters = null;
	private Value returnValue = null;

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
		if (returnValue == null) {
			if(parameters == null)
				returnValue = Interpreter.call(calledFunc, true);
			else
				returnValue = Interpreter.call(calledFunc, true, parameters);
		}
		return returnValue;
	}

	@Override
	public boolean execute(boolean doExecuteNext, ValueHolder... params) {
		print("Calling Function " + calledFunc + (parameters.length == 0 ? "" : " with " + Arrays.toString(parameters)));
		getValue();
		return callNextLine(returnValue == null && doExecuteNext);
	}

}
