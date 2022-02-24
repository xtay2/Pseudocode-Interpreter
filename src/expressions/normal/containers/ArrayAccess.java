package expressions.normal.containers;

import java.util.ArrayList;

import datatypes.ArrayValue;
import datatypes.Value;
import exceptions.runtime.ArrayAccessException;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import types.SuperType;

/** Access at a specific index for example a[19] */
public class ArrayAccess extends Expression implements ValueChanger, MergedExpression {

	private final ArrayList<ValueHolder> indices = new ArrayList<>();
	private Name name;

	public ArrayAccess(int line) {
		super(line, SuperType.MERGED);
	}

	/** [Name] [INDEX] (INDEX), (INDEX)... */
	@Override
	public void merge(Expression... e) {
		if (e.length < 2)
			throw new ArrayAccessException(getOriginalLine(), "Index has to be defined.");
		name = (Name) e[0];
		for (int i = 1; i < e.length; i++)
			indices.add((ValueHolder) e[i]);
	}

	@Override
	public Value getValue() {
		ArrayValue arr = name.getValue().asVarArray();
		for (ValueHolder index : indices)
			return arr.get(index.getValue().asInt().value.intValueExact());
		throw new ArrayAccessException(getOriginalLine(),
				"The specified Array \"" + name.getName() + "\" doesn't contain another array at index " + indices);
	}

	@Override
	public void setValue(Value val) {
		ArrayValue arr = (ArrayValue) name.getValue();
		try {
			for (int i = 0; i < indices.size() - 1; i++)
				arr = (ArrayValue) arr.get(indices.get(i).getValue().asInt().value.intValueExact());
		} catch (ClassCastException e) {
			throw new ArrayAccessException(getOriginalLine(),
					"The specified Array \"" + name.getName() + "\" doesn't contain another array at index " + indices);
		}
		arr.set((int) indices.get(indices.size() - 1).getValue().asInt().value.intValueExact(), val);
	}
}