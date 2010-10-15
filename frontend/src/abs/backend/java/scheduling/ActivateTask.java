package abs.backend.java.scheduling;

import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.runtime.Task;

public class ActivateTask extends ScheduleAction {

	private Task<?> task;

	public ActivateTask(COG cog, Task<?> task) {
	   super(cog);
	   this.task = task;
	}

	@Override
   public String shortString() {
	   return getCOG().getID()+",A,"+task.getID();
   }
	

}
