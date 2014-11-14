package abs.api;

import java.util.HashSet;
import java.util.ServiceLoader;
import java.util.Set;

/**
 * @author Behrooz Nobakht
 * @since 1.0
 */
public class FactoryLoader implements Factory {

	private static final Set<Factory> FACTORIES;

	static {
		FACTORIES = new HashSet<>();
		ServiceLoader<Factory> loader = ServiceLoader.load(Factory.class);
		for (Factory f : loader) {
			FACTORIES.add(f);
		}
	}

	@Override
	public Object create(String fqcn, String... ctorArguments) {
		try {
			Class<?> clazz = Class.forName(fqcn);
			for (Factory f : FACTORIES) {
				if (f.supports(clazz)) {
					try {
						return f.create(fqcn, ctorArguments);
					} catch (Exception e) {
						// ignore
					}
				}
			}
			throw new IllegalArgumentException(fqcn);
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException(fqcn, e);
		}
	}

	@Override
	public boolean supports(Class<?> clazz) {
		return false;
	}

}
