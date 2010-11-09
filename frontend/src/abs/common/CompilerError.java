package abs.common;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

public abstract class CompilerError {
    
	protected File file;
	private String sourceCode; 
	
	public void setSourceLine(String line) {
		sourceLine = line;
	}
	
	public String getHelpMessage() {
		StringBuilder helpMessage = new StringBuilder();
		if (getFileName() != null) {
			helpMessage.append(getFileName()+":");
		}
		
		helpMessage.append(getLine()+":"+getColumn()+":"+getMessage());
		
		final String sourceLine = getSourceLine();
		if (sourceLine != null) {
			helpMessage.append("\n"+sourceLine+"\n");
	        for (int c = 0; c < getColumn()-1; c++) {
	            helpMessage.append('-');
	        }
	        helpMessage.append('^');
			
		}
		
		return helpMessage.toString();
	}

	public String getFileName() {
		if (file != null)
			return file.getName();
		return null;
	}

	
    // source line containing the error, for caching reasons, derived from sourceCode
    private String sourceLine; 
	private String getSourceLine() {
		if (sourceLine != null)
			return sourceLine;
		
		if (file != null) {
			return readLineFromFile();
		}
		
		if (sourceCode != null) {
			return readLineFromSource();
		}
		
		return null;
	}

	private String readLineFromSource() {
		return readLineFromReader(new StringReader(sourceCode));
	}

	private String readLineFromFile() {
		try {
			return readLineFromReader(new FileReader(file));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	private String readLineFromReader(Reader r) {
		try {
			BufferedReader reader = new BufferedReader(r);
			String res = null;
			for (int i=0; i < getLine(); i++) {
				res = reader.readLine();
			}
			return res;
			
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		} 
	}

	public void setFile(String fileName) {
		this.file = new File(fileName); 
	}
	
	public void setFile(File file) {
		this.file = file; 
	}
	
	public void setSourceCode(String sourceCode) {
		this.sourceCode = sourceCode;
	}

    public abstract String getMessage();
    public abstract int getColumn();
    public abstract int getLine();
}
