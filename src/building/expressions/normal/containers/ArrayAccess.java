package building.expressions.normal.containers;

import static building.types.abstractions.SpecificType.MERGED;
import static building.types.specific.FlagType.CONSTANT;

import java.util.List;
import java.util.stream.Collectors;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.possible.multicall.MultiCall;
import building.expressions.possible.multicall.MultiCallableValueChanger;
import errorhandeling.NonExpressionException;
import errorhandeling.PseudocodeException;
import misc.constants.TypeConstants;
import misc.helper.MathHelper;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;

/** Access at a specific index for example a[19] */
public class ArrayAccess extends Expression implements MultiCallableValueChanger {

	private final List<ValueHolder> indices;
	private final Name name;

	public ArrayAccess(int lineID, Name name, List<ValueHolder> indices) {
		super(lineID, MERGED);
		this.name = name;
		this.indices = indices;
		assert name != null && indices != null : "Name and indices cannot be null.";
		if (indices.isEmpty())
			throw new PseudocodeException("MissingIndex", "An array-access has to hold atleast one index.", getDataPath());
	}

	@Override
	public Value getValue() {
		if (indices.size() == 1 && indices.get(0) instanceof MultiCall mc)
			return executeFor(mc.content);
		try {
			return getValue(indices);
		} catch (ArrayIndexOutOfBoundsException iobe) {
			throw new PseudocodeException("ArrayAccess",
					"Index " + indices.stream().map(e -> e.getValue().toString()).collect(Collectors.joining(", ")) + " is out of bounds.",
					getDataPath());
		} catch (NullPointerException npe) {
			throw new AssertionError("Array " + name + " is unitialised.");
		}
	}

	private Value getValue(List<ValueHolder> idxs) {
		try {
			Value v = name.as(TypeConstants.VAR_ARR);
			for (ValueHolder index : idxs)
				v = ((ArrayValue) v).get(MathHelper.valToInt(index));
			return v;
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, getDataPath());
		}
	}

	@Override
	public Value executeFor(ValueHolder[] content) {
		Value[] res = new Value[content.length];
		for (int i = 0; i < content.length; i++)
			res[i] = getValue(List.of(content[i]));
		return ArrayValue.newInstance(res);
	}

	@Override
	public Value setValue(Value val) {
		Variable var = name.getScope().getVar(name);
		if (var.hasFlag(CONSTANT))
			throw new PseudocodeException("ConstantModification",
					"The Array \"" + name.getNameString() + "\" is defined as constant and cannot be changed.", getDataPath());
		if (indices.size() == 1 && indices.get(0) instanceof MultiCall mc)
			return writeFor(val, mc.content);
		try {
			return ((ArrayValue) var.getValue()).set(val, var.getNameString(), getDataPath(),
					indices.toArray(new ValueHolder[indices.size()]));
		} catch (ClassCastException e) {
			throw new PseudocodeException("InvalidDepth",
					"The specified Array \"" + name.getName() + "\" doesn't contain another array at index " + indices, getDataPath());
		}
	}

	@Override
	public ArrayValue writeFor(Value val, ValueHolder[] content) {
		Variable var = name.getScope().getVar(name);
		Value[] previous = new Value[content.length];
		for (int i = 0; i < content.length; i++)
			previous[i] = ((ArrayValue) var.getValue()).set(val, var.getNameString(), getDataPath(), content[i]);
		return ArrayValue.newInstance(previous);
	}

	@Override
	public Name getName() {
		return name;
	}

}