/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.utils;

import java.awt.Color;

public class ColorUtils {
    public static final Color PSYCHEDELIC_PURPLE = new Color(221, 0, 255);

    public static Color setSaturation(Color color, float saturation) {
        float[] hsb = toHSB(color);
        return new Color(Color.HSBtoRGB(hsb[0], saturation, hsb[2]));
    }

    private static float[] toHSB(Color color) {
        return Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(), null);
    }

    public static Color setSatAndBright(Color color, float saturation, float brightness) {
        float[] hsb = toHSB(color);
        return new Color(Color.HSBtoRGB(hsb[0], saturation, brightness));
    }

}
