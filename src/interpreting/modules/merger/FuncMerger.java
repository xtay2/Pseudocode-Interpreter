package interpreting.modules.merger;

import static building.types.specific.BuilderType.CLOSE_BRACKET;
import static building.types.specific.BuilderType.COMMA;
import static building.types.specific.BuilderType.EXPECTED_RETURN_TYPE;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import building.expressions.main.functions.Definition;
import building.expressions.main.functions.Function;
import building.expressions.main.functions.MainFunction;
import building.expressions.main.functions.NativeFunction;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.types.abstractions.SuperType;
import building.types.specific.DataType;

public abstract class FuncMerger extends SuperMerger {

	/** [FUNC] [NAME] [(] (?[?TYPE] [PARAM] [,]) [)] [EXPECTED_RETURN] [EXPECTED_TYPE] [OPEN_SCOPE] */
	public static Definition buildFunc(boolean isNative) {
		line.remove(0);
		Name name = buildName();
		line.remove(0); // OpenBrack
		// PARAMETERS
		if (isNative)
			return new NativeFunction(lineID, name, buildNativeParams(), buildReturnType());
		else
			return new Function(lineID, name, buildFuncParams(), buildReturnType(), (OpenBlock) build());
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

	private static LinkedHashMap<Name, DataType> buildFuncParams() {
		LinkedHashMap<Name, DataType> params = new LinkedHashMap<>();
		if (line.get(0).is(CLOSE_BRACKET)) {
			line.remove(0); // Closebrack
			return params;
		}
		do {
			DataType pT = null;
			if (line.get(0).is(SuperType.DATA_TYPE))
				pT = buildExpType();
			else
				pT = DataType.VAR;
			params.put(buildName(), pT);
		} while (line.remove(0).is(COMMA)); // Removes Comma / Closebrack
		return params;
	}

	/** ([->] [TYPE])? */
	private static DataType buildReturnType() {
		if (!line.isEmpty() && line.get(0).is(EXPECTED_RETURN_TYPE)) {
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
