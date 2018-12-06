package Foreign;

public class Parser_fli extends Parser_c {
	
	public ABSInteger fli_intFromString(ABSString toParse) {
		int parsed = Integer.parseInt(toParse.getString());
		return ABSInteger.fromInt(parsed);
	}
}
