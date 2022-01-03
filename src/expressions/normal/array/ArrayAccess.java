package expressions.normal.array;

import java.util.ArrayList;

import datatypes.ArrayValue;
import datatypes.Value;
import exceptions.runtime.ArrayAccessException;
import expressions.normal.Name;
import expressions.special.Expression;
import expressions.special.ValueHolder;
import helper.Output;
import interpreter.VarManager;

/** Access at a specific index for example a[19] */
public class ArrayAccess extends Expression implements ValueHolder {

	ArrayList<ValueHolder> indices;
	public final Name name;

	public ArrayAccess(int line, Name array, ArrayList<ValueHolder> indices) {
		super(line);
		if (indices.isEmpty())
			throw new ArrayAccessException("Index has to be defined.");
		this.name = array;
		this.indices = indices;
	}

	@Override
	public Value getValue() {
		Value v = VarManager.get(name.getName()).getValue().asVarArray();
		try {
			for (ValueHolder index : indices)
				v = ((ArrayValue) v).get((int) index.getValue().asInt().rawInt());
		} catch (ClassCastException e) {
			throw new ArrayAccessException(
					"The specified Array \"" + name.getName() + "\" doesn't contain another array at index " + indices);
		}
		return v;
	}

	public void setValue(Value var) {
		ArrayValue arr = (ArrayValue) VarManager.get(name.getName()).getValue();
		try {
			for (int i = 0; i < indices.size() - 1; i++)
				arr = (ArrayValue) arr.get((int) indices.get(i).getValue().asInt().rawInt());
		} catch (ClassCastException e) {
			throw new ArrayAccessException(
					"The specified Array \"" + name.getName() + "\" doesn't contain another array at index " + indices);
		}
		arr.set((int) indices.get(indices.size() - 1).getValue().asInt().rawInt(), var);
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "array-access";
	}
}
