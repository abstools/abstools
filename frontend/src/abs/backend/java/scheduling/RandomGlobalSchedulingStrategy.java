package abs.backend.java.scheduling;

import java.util.List;
import java.util.Random;

public class RandomGlobalSchedulingStrategy implements GlobalSchedulingStrategy {
    Random random = new Random();
    
    @Override
    public ScheduleAction choose(ScheduleOptions options) {
        ScheduleAction a = options.allOptions().get(random.nextInt(options.numOptions()));
        System.out.println("Choosing "+a.shortString()+" from "+shortStringList(options.allOptions()));
        return a;
    }

    private String shortStringList(List<ScheduleAction> allOptions) {
        StringBuilder res = new StringBuilder();
        res.append("{");
        boolean first = true;
        for (ScheduleAction a : allOptions) {
            if (first) first = false;
            else res.append(", ");
            res.append(a.shortString());
        }
        res.append("}");
        return res.toString();
    }

}
