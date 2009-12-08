package tests;


import ast.*;
import parser.*;
import java.io.*;

public class ParserTest {



    public static void main(String[] args) throws Exception {

	System.out.println("Testing parser");
	if (args.length == 0) {
	    	System.out.println("Usage java ParserTest filename");
	} else {
	    System.out.println("Trying to parse: " + java.util.Arrays.toString(args));
	Program p = parse(args);
	System.out.println("Done parsing. Result:");
	System.out.println(p);
	p.dumpTree("  ", System.out);
	System.out.println("Bye");
     }
    }

               
  protected static Program parse(String args[]) throws Exception {
      Reader reader = getReader(args);
      ABSParser parser = new ABSParser();
      ABSScanner scanner = new ABSScanner(reader);
      Program p = (Program)parser.parse(scanner);
      reader.close();
      return p; 
  }
    
    private static Reader getReader(String[] args) {
	Reader r = null;
	if (args.length != 1) {
	    r = new InputStreamReader(System.in);
	} else {
	    try {
		r = new FileReader(args[0]);
	    } catch (FileNotFoundException e1) {
		System.err.println("Dumper: file " + args[0] + " not found");
	    }
	}
	return r;
    }



}
