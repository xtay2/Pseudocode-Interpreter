package files;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;

public final class FileManager {

	/**
	 * Write a string into the textfile.
	 * 
	 * @param content is the string.
	 * @param path    is the relative path, where the file is stored.
	 */
	public static void writeFile(String content, String path) {
		BufferedWriter writer;
		try {
			writer = new BufferedWriter(new FileWriter(path));
			writer.write(content);
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Convert a textfile to a string.
	 * 
	 * @param path is the relative path, where the file is stored.
	 * @return the content of the file as a string.
	 */
	public static String readFile(String path) {
		String out = "";
		try (BufferedReader br = new BufferedReader(new FileReader(path))) {
			out = br.readLine();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return out;
	}

	/**
	 * Convert a textfile to an array of string.
	 * 
	 * @param path is the relative path, where the file is stored.
	 * @return the content of the file, line by line, as a string array.
	 */
	public static String[] fileToLineArray(String path) {
		String[] programm = new String[countLinesInFile(path)];
		try (BufferedReader br = new BufferedReader(new FileReader(path))) {
			for (int i = 0; i < programm.length; i++) {
				programm[i] = br.readLine();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return programm;
	}

	/**
	 * Count the number of lines in a textfile.
	 * 
	 * @param path is the relative path, where the file is stored.
	 * @return the linecount as int
	 */
	public static int countLinesInFile(String path) {
		try (FileReader input = new FileReader(path); LineNumberReader count = new LineNumberReader(input);) {
			count.skip(Long.MAX_VALUE);
			return count.getLineNumber();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return -1;
	}
}
