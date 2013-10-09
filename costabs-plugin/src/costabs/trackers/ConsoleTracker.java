package costabs.trackers;

import java.util.ArrayList;
import java.util.Collection;

import costabs.beans.Command;
import costabs.console.ConsoleHandler;

public class ConsoleTracker extends CommandTracker{

	public ConsoleTracker(Command command) {
		super(command);
	}

	@Override
	public void track() {
		ConsoleHandler.write(getLevel(),getText());
	}

	@Override
	public void clean() {
	}

	public static Collection<String> getMarkers() {
		return new ArrayList<String>();
	}

}
