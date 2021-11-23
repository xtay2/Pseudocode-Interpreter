import files.FileManager;
import programreader.parser.Parser;

public class Main {
	public static void main(String[] args) {
		Parser.parseToExpressions(FileManager.fileToLineArray("Files/Program.txt"));
	}
}
