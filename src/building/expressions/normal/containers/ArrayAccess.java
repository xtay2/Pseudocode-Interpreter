package building.expressions.normal.containers;

import static building.types.abstractions.SpecificType.MERGED;
import static building.types.specific.FlagType.CONSTANT;
import static building.types.specific.datatypes.ArrayType.VAR_ARRAY;

import java.util.List;
import java.util.stream.Collectors;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.possible.multicall.MultiCall;
import building.expressions.possible.multicall.MultiCallableValueChanger;
import interpreting.exceptions.IllegalCodeFormatException;
import misc.helper.MathHelper;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;
import runtime.exceptions.ArrayAccessException;
import runtime.exceptions.DeclarationException;

/** Access at a specific index for example a[19] */
public class ArrayAccess extends Expression implements MultiCallableValueChanger {

	private final List<ValueHolder> indices;
	private final Name name;

	public ArrayAccess(int lineID, Name name, List<ValueHolder> indices) {
		super(lineID, MERGED);
		this.name = name;
		this.indices = indices;
		if (name == null || indices == null)
			throw new AssertionError("Name and indices cannot be null.");
		if (indices.isEmpty())
			throw new IllegalCodeFormatException(getOriginalLine(), "An array-access has to hold atleast a one-dimensional index.");
	}

	@Override
	public Value getValue() {
		if (indices.size() == 1 && indices.get(0) instanceof MultiCall mc)
			return executeFor(mc.content);
		try {
			return getValue(indices);
		} catch (ArrayIndexOutOfBoundsException iobe) {
			throw new ArrayAccessException(getOriginalLine(),
					"Index " + indices.stream().map(e -> e.getValue().toString()).collect(Collectors.joining(", ")) + " is out of bounds.");
		} catch (NullPointerException npe) {
			throw new AssertionError("Array " + name + " is unitialised.");
		}
	}

	private Value getValue(List<ValueHolder> idxs) {
		Value v = name.getValue().as(VAR_ARRAY);
		for (ValueHolder index : idxs)
			v = ((ArrayValue) v).get(MathHelper.valToInt(index));
		return v;
	}

	@Override
	public Value executeFor(ValueHolder[] content) {
		ValueHolder[] res = new ValueHolder[content.length];
		for (int i = 0; i < content.length; i++)
			res[i] = getValue(List.of(content[i]));
		return new ArrayValue(VAR_ARRAY, res);
	}

	@Override
	public Value setValue(Value val) {
		Variable var = name.getScope().getVar(name.getNameString(), getOriginalLine());
		if (var.hasFlag(CONSTANT))
			throw new DeclarationException(getOriginalLine(),
					"The Array \"" + name.getNameString() + "\" is defined as constant and cannot be changed.");
		if (indices.size() == 1 && indices.get(0) instanceof MultiCall mc)
			return writeFor(val, mc.content);
		try {
			return ((ArrayValue) var.getValue()).set(val, indices.toArray(new ValueHolder[indices.size()]));
		} catch (ClassCastException e) {
			throw new ArrayAccessException(getOriginalLine(),
					"The specified Array \"" + name.getName() + "\" doesn't contain another array at index " + indices);
		}
	}

	@Override
	public ArrayValue writeFor(Value val, ValueHolder[] content) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Name getName() {
		return name;
	}

}