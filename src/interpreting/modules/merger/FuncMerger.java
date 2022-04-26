package interpreting.modules.merger;

import static building.types.specific.BuilderType.*;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;

import building.expressions.abstractions.Range;
import building.expressions.main.functions.Definition;
import building.expressions.main.functions.Function;
import building.expressions.main.functions.MainFunction;
import building.expressions.main.functions.NativeFunction;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.types.abstractions.SuperType;
import building.types.specific.datatypes.ArrayType;
import building.types.specific.datatypes.DataType;
import building.types.specific.datatypes.SingleType;

public abstract class FuncMerger extends SuperMerger {

	/** [FUNC] [NAME] [(] (?[?TYPE] [PARAM] [,]) [)] [EXPECTED_RETURN] [EXPECTED_TYPE] [OPEN_SCOPE] */
	public static Definition buildFunc(boolean isNative) {
		line.remove(0);
		Name name = buildName();
		line.remove(0); // OpenBrack
		// PARAMETERS
		if (isNative)
			return new NativeFunction(lineID, name, buildNativeParams(), buildReturnType(), checkIfNullAllowed());
		else
			return new Function(lineID, name, buildFuncParams(), buildReturnType(), checkIfNullAllowed(), (OpenBlock) build());
	}

	private static List<DataType> buildNativeParams() {
		List<DataType> params = new ArrayList<>();
		while (line.get(0).type instanceof DataType || line.get(0).is(COMMA)) {
			if (line.get(0).type instanceof DataType)
				params.add(buildExpType());
			else
				line.remove(0);
		}
		line.remove(0); // Closebrack
		return params;
	}

	private static LinkedHashMap<Name, Entry<DataType, Boolean>> buildFuncParams() {
		LinkedHashMap<Name, Entry<DataType, Boolean>> params = new LinkedHashMap<>();
		if (line.get(0).is(CLOSE_BRACKET)) {
			line.remove(0); // Closebrack
			return params;
		}
		do {
			DataType pT = null;
			boolean allowNull = false;
			if (line.get(0).is(SuperType.DATA_TYPE)) {
				pT = buildExpType();
				if (line.get(0).is(MAYBE)) {
					line.remove(0);
					allowNull = true;
				}
				if (line.get(0).is(ARRAY_START) && line.get(1).is(ARRAY_END)) {
					line.remove(0);
					line.remove(0);
					pT = ArrayType.create((SingleType) pT, Range.UNBOUNDED);
				}
			} else
				pT = SingleType.VAR;
			params.put(buildName(), new SimpleEntry<>(pT, allowNull));
		} while (line.remove(0).is(COMMA)); // Removes Comma / Closebrack
		return params;
	}

	/** ([->] [TYPE])? */
	private static DataType buildReturnType() {
		if (!line.isEmpty() && line.get(0).is(ARROW_R)) {
			line.remove(0); // Arrow
			return buildExpType();
		}
		return null;
	}

	/** [MAIN] [{] */
	public static MainFunction buildMain() {
		line.remove(0);
		return new MainFunction(lineID, (OpenBlock) build());
	}
}
