package expressions.normal.containers;

import static types.specific.FlagType.CONSTANT;

import java.util.List;
import java.util.stream.Collectors;

import datatypes.ArrayValue;
import datatypes.Value;
import exceptions.runtime.ArrayAccessException;
import exceptions.runtime.DeclarationException;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import types.SuperType;

/** Access at a specific index for example a[19] */
public class ArrayAccess extends Expression implements ValueChanger {

	private final List<ValueHolder> indices;
	private final Name name;

	public ArrayAccess(int lineID, Name name, List<ValueHolder> indices) {
		super(lineID, SuperType.MERGED);
		this.name = name;
		this.indices = indices;
		if (name == null || indices == null)
			throw new AssertionError("Name and indices cannot be null.");
	}

	@Override
	public Value getValue() {
		Value v = name.getValue().asVarArray();
		try {
			for (ValueHolder index : indices)
				v = v.asVarArray().get(index.getValue().asInt().value.intValueExact());
			return v;
		} catch (ArrayIndexOutOfBoundsException iobe) {
			throw new ArrayAccessException(getOriginalLine(),
					"Index " + indices.stream().map(e -> e.getValue().toString()).collect(Collectors.joining(", "))
							+ " is out of bounds for length " + v.asVarArray().length());
		}
	}

	@Override
	public void setValue(Value val) {
		Variable v = name.getScope().get(name.getNameString(), getOriginalLine());
		if (v.hasFlag(CONSTANT))
			throw new DeclarationException(getOriginalLine(),
					"The Array \"" + name.getNameString() + "\" is defined as constant and cannot be changed.");
		ArrayValue arr = v.getValue().asVarArray();
		try {
			for (int i = 0; i < indices.size() - 1; i++)
				arr = (ArrayValue) arr.get(indices.get(i).getValue().asInt().value.intValueExact());
		} catch (ClassCastException e) {
			throw new ArrayAccessException(getOriginalLine(),
					"The specified Array \"" + name.getName() + "\" doesn't contain another array at index " + indices);
		}
		arr.set(indices.get(indices.size() - 1).getValue().asInt().value.intValueExact(), val);
	}

	@Override
	public Name getName() {
		return name;
	}
}