package abs.backend.java.utils;

public class StringUtil {

    public static String iterableToString(Iterable<?> it, String sep) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (Object o : it) {
            if (first)
                first = false;
            else
                sb.append(sep);
            sb.append(o.toString());

        }
        return sb.toString();
    }

    public static <A, B extends ToString<A>> String iterableToString(Iterable<A> it, String sep, B toString) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (A a : it) {
            if (first)
                first = false;
            else
                sb.append(sep);
            sb.append(toString.toString(a));

        }
        return sb.toString();
    }

}
