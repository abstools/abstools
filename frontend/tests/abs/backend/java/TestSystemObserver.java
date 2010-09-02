package abs.backend.java;

import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.COGView;
import abs.backend.java.observing.EmptyTaskListener;
import abs.backend.java.observing.GuardObs;
import abs.backend.java.observing.ObjectCreationListener;
import abs.backend.java.observing.ObjectView;
import abs.backend.java.observing.SystemObserver;
import abs.backend.java.observing.TaskListener;
import abs.backend.java.observing.TaskView;

public class TestSystemObserver implements SystemObserver, ObjectCreationListener {

    @Override
    public void systemStarted(TaskView mainTask, COGView mainCOG) {
        mainCOG.registerObjectCreationListener(this);
        mainCOG.getScheduler().registerTaskActionListener(new EmptyTaskListener() {
            @Override
            public void taskCreated(TaskView task) {
                System.out.print("TASK CREATED: "+task.getTarget().getClassName()+"."+task.getMethodName()+"(");
                int i = 0;
                for (ABSValue v : task.getArgs()) {
                    if (i > 0) System.out.print(", ");
                    System.out.print(v);
                    i++;
                }
                System.out.println(")");
            }
        });
        
        mainTask.registerTaskListener(new EmptyTaskListener() {
            @Override
            public void taskFinished(TaskView task) {
                System.out.println("SYSTEM TERMINATED");
            }
            
        });
        
        System.out.println("SYSTEM STARTED");
    }

    @Override
    public void objectCreated(ObjectView o, boolean newCOG) {
        System.out.println("OBJECT CREATED: "+o.getClassName());
        
        if (o.getClassName().equals("FieldClass")) {
            try {
                ABSString s =  (ABSString) o.getFieldValue("field");
                System.out.println("FIELD VALUE="+s.getString());
            } catch (NoSuchFieldException e) {
                e.printStackTrace();
            }
        }
    }
    
    

}
