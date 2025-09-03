package org.abs_models.backend.java.lib.runtime;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.abs_models.backend.java.lib.types.ABSAlgebraicDataType;
import org.abs_models.backend.java.lib.types.ABSInterface;
import org.apfloat.Aprational;

/**
 * A mirror for the ABS.DC.DeploymentComponent class.

 * This class only exists once we compile a Java model, since it is defined in
 * the file {@code abslang.abs}.  We use reflection to find the ABS methods
 * that we need, and wrap them in friendlier methods as necessary.
 */
public class ABSDCMirror {
    final ABSInterface dc;
    
    public ABSInterface getWrappedDC() {
        return dc;
    }

    public ABSDCMirror(ABSInterface abs_dc) {
        if (!CLASS_DC.isInstance(abs_dc)) {
            throw new RuntimeException("Trying to register a non-DC object as DC");
        }
        this.dc = abs_dc;
    }

    /**
     * Consume some CPU resources on the deployment component, and return the
     * amount consumed.  In case the deployment component has infinite CPU
     * resources, consume all that is needed; otherwise, consume as much as
     * available.
     *
     * @param needed_cpu The amount of CPU needed.
     * @return The amount of CPU consumed, always less than or equal to
     * {@code needed_cpu}.
     */
    public Aprational consumeCPU(Aprational needed_cpu) {
        try {
            try {
                DC_CHECK_SAME_COG.setBoolean(dc, false);
                return (Aprational)DC_CONSUME_COST.invoke(dc, needed_cpu);
            } finally {
                DC_CHECK_SAME_COG.setBoolean(dc, true);
            }
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Update DC state: advance time by one tick.
     */
    public void advanceTimeBy1Tick() {
        try {
            try {
                DC_CHECK_SAME_COG.setBoolean(dc, false);
                DC_ADVANCE_TIME_BY_1_TICK.invoke(dc);
            } finally {
                DC_CHECK_SAME_COG.setBoolean(dc, true);
            }
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

    private Double getCreationTime() {
        ABSAlgebraicDataType creationTimeRaw;
	try {
	    creationTimeRaw = (ABSAlgebraicDataType)GET_FIELD_VALUE.invoke(dc, "creationTime");
	} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            throw new RuntimeException(e);
	}
        @SuppressWarnings("rawtypes")
	    Map m = (Map)creationTimeRaw.toJson();
        Double result = (Double)m.get("timeValue");
        return result;
    }

    private List<Number> getCpuHistory() {
        ABSAlgebraicDataType cpuHistoryRaw;
	try {
	    cpuHistoryRaw = (ABSAlgebraicDataType)GET_FIELD_VALUE.invoke(dc, "cpuhistory");
	} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            throw new RuntimeException(e);
	}
        @SuppressWarnings("rawtypes")
            List<Number> result = (List)cpuHistoryRaw.toJson();
        return result.reversed();
    }

    private List<Number> getCpuTotalsHistory() {
        ABSAlgebraicDataType cpuTotalsHistoryRaw;
	try {
	    cpuTotalsHistoryRaw = (ABSAlgebraicDataType)GET_FIELD_VALUE.invoke(dc, "cpuhistorytotal");
	} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            throw new RuntimeException(e);
	}
        @SuppressWarnings("rawtypes")
            List<Number> result = (List)cpuTotalsHistoryRaw.toJson();
        return result.reversed();
    }

    /**
     * Get CPU history as a list of lists.  Each sublist contains two
     * or three elements: the time of recording the value (starting
     * with the creation time of the deployment component and
     * increasing by 1), the consumed CPU in that time slot, and (if
     * applicable), the total available CPU in that time slot.
     */
    public List<List<Number>> getCpuHistories() {
        Double ct = getCreationTime();
        List<Number> cpuHistory = getCpuHistory();
        List<Number> cpuTotalsHistory = getCpuTotalsHistory();
        List<List<Number>> result = IntStream
            .range(0, cpuHistory.size())
            .mapToObj(i -> (cpuTotalsHistory.isEmpty()
                ? List.of(ct + i, cpuHistory.get(i))
                : List.of(ct + i, cpuHistory.get(i), cpuTotalsHistory.get(i))))
            .collect(Collectors.toList());
        return result;
    }

    public String getDescription() {
	    try {
	        return (String)GET_FIELD_VALUE.invoke(dc, "description");
	    } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
	    }
    }

    // Find class ABS.DC.DeploymentComponent and needed methods, and the
    // InfRat datatype.
    static Class<?> CLASS_DC;      // ABS.DC.DeploymentComponent
    static Field DC_CHECK_SAME_COG; // ABS.DC.DeploymentComponent.__checkSameCog
    static Method DC_CONSUME_COST; // ABS.DC.DeploymentComponent#consumeCost
    static Method DC_ADVANCE_TIME_BY_1_TICK; // ABS.DC.DeploymentComponent#advanceTimeBy1Tick
    static Method GET_FIELD_VALUE;           // ABS.DC.DeploymentComponent#getFieldValue
    static {
        try {
            CLASS_DC = Class.forName("ABS.DC.DeploymentComponent_c");
            DC_CHECK_SAME_COG = ABSObject.class.getDeclaredField("__checkSameCog");
            DC_CONSUME_COST = CLASS_DC.getDeclaredMethod("consumeCost", Aprational.class);
            DC_ADVANCE_TIME_BY_1_TICK = CLASS_DC.getDeclaredMethod("advanceTimeBy1Tick");
            GET_FIELD_VALUE = CLASS_DC.getDeclaredMethod("getFieldValue", java.lang.String.class);
            GET_FIELD_VALUE.setAccessible(true); // override `protected` flag
        } catch (ClassNotFoundException | NoSuchMethodException | NoSuchFieldException e) {
            throw new RuntimeException("Failed to find something from the ABS standard library: " + e.getMessage());
        }
    }
}
