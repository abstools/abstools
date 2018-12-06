package Foreign;

public class Sleeper_fli extends Sleeper_c {
	public ABSUnit fli_sleep(ABSInteger absTime) {
		int time = absTime.toInt();
		try {
			Thread.sleep(time);
		} catch (InterruptedException e) {
			return ABSUnit.UNIT;
		}
		return ABSUnit.UNIT;
	}
}
