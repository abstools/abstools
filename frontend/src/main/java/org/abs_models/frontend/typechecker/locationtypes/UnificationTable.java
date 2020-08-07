package org.abs_models.frontend.typechecker.locationtypes;

import java.util.HashMap;
import java.util.Map;

public class UnificationTable {
    private Map<LocationTypeVar, LocationTypeVar> table = new HashMap<>();

    public LocationTypeVar resolve(LocationTypeVar lv) {
        while (table.containsKey(lv)) {
            lv = table.get(lv);
        }
        return lv;
    }

    /**
     * Tries to set lv1 equal to lv2
     * @param lv1
     * @param lv2
     */
    public void unifiy(LocationTypeVar lv1, LocationTypeVar lv2) throws UnificationException {
        lv1 = resolve(lv1);
        lv2 = resolve(lv2);
        if (lv1.isLocationType() && lv2.isLocationType()) {
            // Both are resolved, check if they fit
            LocationType lt1 = lv1.asLocationType();
            LocationType lt2 = lv2.asLocationType();

            if (!lt1.isSubtypeOf(lt2)) {
                throw new UnificationException();
            }
            return;
        }

        if (lv1.isLocationType()) {
            // Only lv1 is resolved
            table.put(lv2, lv1);
            return;
        }
        if (lv2.isLocationType()) {
            // Only lv2 is resolved
            table.put(lv1, lv2);
            return;
        }

        // Both are not resolved
        table.put(lv1, lv2);
    }
}
