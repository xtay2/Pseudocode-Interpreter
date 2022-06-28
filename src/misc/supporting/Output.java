package misc.supporting;

import java.util.*;

public final class Output {
	
	public static boolean debugMode = false;
	
	public static final String UNDERLINE = "\n----------------------";
	
	public static void print(Object text) {
		if (debugMode)
			System.out.println(text);
	}
	
	public static void printAll(String title, Collection<?> collection) {
		if (debugMode) {
			System.out.println("---" + title + "---");
			if (collection.isEmpty())
				System.out.println("<none>");
			else {
				collection.forEach(System.out::println);
				System.out.println();
			}
		}
	}
	
}
