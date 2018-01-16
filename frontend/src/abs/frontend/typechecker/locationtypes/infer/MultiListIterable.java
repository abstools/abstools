/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.locationtypes.infer;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class MultiListIterable<T> implements Iterable<T> {
    Iterable<T>[] args;
    public MultiListIterable(Iterable<T>... args) {
        if (args.length == 0) {
            throw new IllegalArgumentException();
        }
        this.args = args;
    }

    @Override
    public Iterator<T> iterator() {
        return new Iterator<T>() {
            int currentList = 0;
            Iterator<T> currentIt = args[0].iterator();

            @Override
            public boolean hasNext() {
                setNextIterator();
                return currentIt != null;
            }

            private void setNextIterator() {
                if (currentIt == null)
                    return;
                while (!currentIt.hasNext()) {
                    currentList++;
                    if (currentList == args.length) {
                        currentIt = null;
                        return;
                    }
                    currentIt = args[currentList].iterator();
                }
            }

            @Override
            public T next() {
                setNextIterator();
                return currentIt.next();
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }

        };
    }


    public static <T> List<T> fromIterable(Iterable<T> it) {
        List<T> result = new ArrayList<>();
        for (T t : it) {
            result.add(t);
        }
        return result;
    }

    public String toString() {
        return fromIterable(this).toString();
    }

}
