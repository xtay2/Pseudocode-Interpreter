package expressions.main.functions;

import static helper.Output.print;
import static parsing.program.ExpressionType.OPEN_SCOPE;

import java.util.List;

import exceptions.parsing.UnexpectedFlagException;
import expressions.abstractions.Expression;
import expressions.abstractions.MergedExpression;
import expressions.abstractions.ValueHolder;
import expressions.normal.brackets.OpenScope;
import expressions.normal.containers.Name;
import interpreter.Interpreter;
import interpreter.VarManager;
import parsing.program.KeywordType;

public class MainFunction extends Function implements MergedExpression {

	public MainFunction(int line) {
		super(line);
		setExpectedExpressions(OPEN_SCOPE);
		name = new Name(KeywordType.MAIN.toString(), line);
	}

	@Override
	public void merge(Expression... e) {
		if (e.length != 1 || e[0] == null)
			throw new AssertionError("Merge on a main-func has to contain the opened scope.");
		openScope = (OpenScope) e[0];
	}

	@Override
	public void setFlags(List<KeywordType> flags) throws UnexpectedFlagException {
		throw new UnexpectedFlagException(getOriginalLine(), "The main-function doesn't take any flags.");
	}

	@Override
	public boolean execute(ValueHolder... params) {
		print("Executing Main.");
		VarManager.registerScope(this);
		Interpreter.execute(lineIdentifier + 1);
		VarManager.deleteScope(this);
		return false;
	}
}

