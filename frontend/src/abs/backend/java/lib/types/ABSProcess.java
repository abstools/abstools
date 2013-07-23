/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.types;

public class ABSProcess extends ABSBuiltInDataType {

    // set: pid, method, arrival, cost, deadline, start, finish, critical, value

    private int pid;
    private String methodName;
    private long arrivalTime;
    private long cost;
    private long deadline;
    private long startTime;
    private long finishTime;
    private boolean critical;
    private int value;
    
    public ABSProcess() {
        super("ABSProcess");
    }

    public ABSProcess(int pid, String method, long arrival, long cost, long deadline, long start, long finish, boolean crit, int value) {
        super("ABSProcess");
        this.pid = pid;
        this.methodName = method;
        this.arrivalTime = arrival;
        this.cost = cost;
        this.deadline = deadline;
        this.startTime = start;
        this.finishTime = finish;
        this.critical = crit;
        this.value = value;
    }
    
    public int getPid() {
        return pid;
    }
    public String getMethodName() {
        return methodName;
    }
    public long getArrivalTime() {
        return arrivalTime;
    }
    public long getCost() {
        return cost;
    }
    public long getDeadline() {
        return deadline;
    }
    public long getStartTime() {
        return startTime;
    }
    public long getFinishTime() {
        return finishTime;
    }
    public boolean isCritical() {
        return critical;
    }
    public int getValue() {
        return value;
    }
    
    public String toString() {
        return String.format("%1$s: %2$d,%3$s,%4$d,%5$d,%6$d,%7$d,%8$d,%9$b,%10$d", 
                getConstructorName(), pid, methodName, arrivalTime, cost, deadline, startTime, finishTime, critical, value);
    }
    
    public ABSBool gt(ABSValue o) {
        if (o.getClass() == ABSProcess.class) {
            return ABSBool.fromBoolean(this.pid > ((ABSProcess)o).getPid());
        } else {
            // type error, not reached
            return ABSBool.FALSE;
        }
    }

    public ABSBool lt(ABSValue o) {
        if (o.getClass() == ABSProcess.class) {
            return ABSBool.fromBoolean(this.pid < ((ABSProcess)o).getPid());
        } else {
            // type error, not reached
            return ABSBool.FALSE;
        }
    }


}
