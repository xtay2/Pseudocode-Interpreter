package expressions.normal.array;

import java.util.ArrayList;

import datatypes.ArrayValue;
import datatypes.Value;
import exceptions.runtime.ArrayAccessException;
import expressions.normal.Expression;
import expressions.normal.Name;
import expressions.special.MergedExpression;
import expressions.special.ValueChanger;
import expressions.special.ValueHolder;

/** Access at a specific index for example a[19] */
public class ArrayAccess extends Expression implements ValueChanger, MergedExpression {

	private final ArrayList<ValueHolder> indices = new ArrayList<>();
	private Name name;

	public ArrayAccess(int line) {
		super(line);
	}

	@Override
	public void merge(Expression... e) {
		name = (Name) e[0];
		if (e.length < 2)
			throw new ArrayAccessException(getOriginalLine(), "Index has to be defined.");
		for (int i = 1; i < e.length; i++)
			indices.add((ValueHolder) e[i]);
	}

	@Override
	public Value getValue() {
		ArrayValue arr = name.getValue().asVarArray();
		try {
			for (ValueHolder index : indices)
				return arr.get((int) index.getValue().asInt().rawInt());
		} catch (ClassCastException e) {
			e.printStackTrace();
		}
		throw new ArrayAccessException(getOriginalLine(),
				"The specified Array \"" + name.getName() + "\" doesn't contain another array at index " + indices);
	}

	@Override
	public void setValue(Value val) {
		ArrayValue arr = (ArrayValue) name.getValue();
		try {
			for (int i = 0; i < indices.size() - 1; i++)
				arr = (ArrayValue) arr.get((int) indices.get(i).getValue().asInt().rawInt());
		} catch (ClassCastException e) {
			throw new ArrayAccessException(getOriginalLine(),
					"The specified Array \"" + name.getName() + "\" doesn't contain another array at index " + indices);
		}
		arr.set((int) indices.get(indices.size() - 1).getValue().asInt().rawInt(), val);
	}
}