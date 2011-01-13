package abs.backend.java.debugging;

import java.awt.Color;

import abs.backend.java.utils.ColorUtils;

public enum TaskState{
    READY(ColorUtils.setSaturation(Color.YELLOW, 0.5f)), SUSPENDED(ColorUtils.setSaturation(Color.ORANGE, 0.5f)), RUNNING(
            ColorUtils.setSaturation(Color.GREEN, 0.5f)), FINISHED(Color.LIGHT_GRAY), DEADLOCKED(Color.red), ASSERTION_FAILED(
            ColorUtils.PSYCHEDELIC_PURPLE), EXCEPTION(ColorUtils.PSYCHEDELIC_PURPLE), BLOCKED(ColorUtils.setSaturation(
            Color.RED, 0.5f));
    public final Color color;

    TaskState(Color c) {
        this.color = c;
    }
}