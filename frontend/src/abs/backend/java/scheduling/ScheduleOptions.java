package abs.backend.java.scheduling;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public class ScheduleOptions implements Iterable<ScheduleAction> {
    private List<ScheduleAction> actions = new ArrayList<ScheduleAction>();
    
    public int numOptions() {
        return actions.size();
    }
    
    public void addOption(ScheduleAction a) {
        actions.add(a);
    }
    
    @Override
    public Iterator<ScheduleAction> iterator() {
        return actions.iterator();
    }
    
    public List<ScheduleAction> allOptions() {
        return Collections.unmodifiableList(actions);
    }

    public void removeOption(ScheduleAction next) {
        actions.remove(next);
    }
    
    
}
