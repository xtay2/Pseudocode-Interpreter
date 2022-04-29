package interpreting.modules.merger;

import static building.types.specific.BuilderType.ARROW_R;
import static building.types.specific.BuilderType.CLOSE_BRACKET;
import static building.types.specific.BuilderType.COMMA;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;

import building.expressions.main.functions.Definition;
import building.expressions.main.functions.Function;
import building.expressions.main.functions.MainFunction;
import building.expressions.main.functions.NativeFunction;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.types.abstractions.SuperType;
import building.types.specific.datatypes.DataType;
import building.types.specific.datatypes.SingleType;

public abstract class FuncMerger extends SuperMerger {

	/** [FUNC] [NAME] [(] (?[?TYPE] [PARAM] [,]) [)] [EXPECTED_RETURN] [EXPECTED_TYPE] [OPEN_SCOPE] */
	public static Definition buildFunc(boolean isNative) {
		line.remove(0);
		Name name = buildName();
		line.remove(0); // OpenBrack
		// PARAMETERS
		if (isNative) {
			List<DataType> params = buildNativeParams();
			Entry<DataType, Boolean> returnType = buildReturnType();
			return new NativeFunction(lineID, name, params, returnType.getKey(), returnType.getValue());
		}
		LinkedHashMap<Name, Entry<DataType, Boolean>> params = buildFuncParams();
		Entry<DataType, Boolean> returnType = buildReturnType();
		return new Function(lineID, name, params, returnType.getKey(), returnType.getValue(), (OpenBlock) build());

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
				Entry<DataType, Boolean> paramType = buildCastEntry();
				pT = paramType.getKey();
				allowNull = paramType.getValue();
			} else
				pT = SingleType.VAR;
			params.put(buildName(), new SimpleEntry<>(pT, allowNull));
		} while (line.remove(0).is(COMMA)); // Removes Comma / Closebrack
		return params;
	}

	/** ([->] [TYPE])? */
	private static Entry<DataType, Boolean> buildReturnType() {
		if (!line.isEmpty() && line.get(0).is(ARROW_R)) {
			line.remove(0); // Arrow
			return buildCastEntry();
		}
		return new SimpleEntry<DataType, Boolean>(null, false);
	}

	/** [MAIN] [{] */
	public static MainFunction buildMain() {
		line.remove(0);
		return new MainFunction(lineID, (OpenBlock) build());
	}
}
