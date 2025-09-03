package Foreign;

public class Parser_fli extends Parser_c {
	
	public Apint fli_intFromString(ABSString toParse) {
		int parsed = Integer.parseInt(toParse.getString());
		return new Apint(parsed);
	}
}
