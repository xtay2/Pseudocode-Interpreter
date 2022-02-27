package expressions.normal.containers;

import static types.specific.FlagType.CONSTANT;

import java.util.ArrayList;
import java.util.stream.Collectors;

import datatypes.ArrayValue;
import datatypes.Value;
import exceptions.runtime.ArrayAccessException;
import exceptions.runtime.DeclarationException;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueChanger;
import expressions.abstractions.interfaces.ValueHolder;
import types.SuperType;

/** Access at a specific index for example a[19] */
public class ArrayAccess extends Expression implements ValueChanger, MergedExpression {

	private final ArrayList<ValueHolder> indices = new ArrayList<>();
	private Name name;

	public ArrayAccess(int lineID) {
		super(lineID, SuperType.MERGED);
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