package org.abs_models.backend.scala;

import javax.lang.model.element.Element;
import java.util.function.Function;

/**
 * A supplier for {@link JavaWriter} that provides separate
 * writers for every top-level Java {@link Element}. It is a
 * {@link Function} from the type name (String) to the required
 * Java writer.
 */
public interface JavaWriterSupplier extends Function<String, JavaWriter> {

}
