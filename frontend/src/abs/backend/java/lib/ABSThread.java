package abs.backend.java.lib;

public class ABSThread extends Thread {
	private COG cog;
	
	public COG getCOG() {
		return cog;
	}
	
	public void setCOG(COG c) {
		cog = c;
	}
}
