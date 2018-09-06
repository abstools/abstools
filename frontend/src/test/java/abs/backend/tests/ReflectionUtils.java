package abs.backend.tests;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

/**
 * A helper class for gathering instance and static field information for unit
 * test.
 * 
 * @author woner
 * 
 */
public class ReflectionUtils {

    public static Object getField(Object object, String name) {
        return getField(object, object.getClass(), name);
    }

    /**
     * Get the value of a field with {@code name} from {@code object}.
     * 
     * @param <V>
     *            type of the field
     * @param object
     * @param name
     * @return the value of a field with {@code name} from {@code object}.
     * @throws IllegalStateException
     *             if cannot get the specified field from the specified object.
     * 
     * @see Class#getDeclaredField(String)
     * @see Field#get(Object)
     * @see Field#setAccessible(boolean)
     */
    public static Object getField(Object object, Class<?> klass, String name) {
        try {
            Field field = klass.getDeclaredField(name);
            field.setAccessible(true);
            // remove final modifier from field
            int modifier = unfinalize(field);
            Object value = field.get(object);
            setModifier(field, modifier);
            field.setAccessible(false);
            return value;
        } catch (SecurityException e) {
            throw new IllegalStateException("Cannot get field " + name + " from object " + object, e);
        } catch (NoSuchFieldException e) {
            throw new IllegalStateException("Cannot get field " + name + " from object " + object, e);
        } catch (IllegalArgumentException e) {
            throw new IllegalStateException("Cannot get field " + name + " from object " + object, e);
        } catch (IllegalAccessException e) {
            throw new IllegalStateException("Cannot get field " + name + " from object " + object, e);
        }
    }

    public static <O, V> O setField(O object, String name, V value) {
        return setField(object, object.getClass(), name, value);
    }

    /**
     * Set field {@code name} of {@code object} with {@code value}.
     * 
     * @param <O>
     * @param <V>
     * @param object
     * @param name
     * @param value
     * @return
     * @throws IllegalStateException
     *             if cannot set the specified field of the specified object
     *             with the value.
     * 
     * @see Class#getDeclaredField(String)
     * @see Field#set(Object, Object)
     * @see Field#setAccessible(boolean)
     */
    public static <O> O setField(O object, Class<?> klass, String name, Object value) {
        try {
            Field field = klass.getDeclaredField(name);
            field.setAccessible(true);
            // remove final modifier from field
            int modifier = unfinalize(field);
            field.set(object, value);
            setModifier(field, modifier);
            field.setAccessible(false);
            return object;
        } catch (SecurityException e) {
            throw new IllegalStateException("Cannot set field " + name + " from object " + object, e);
        } catch (NoSuchFieldException e) {
            throw new IllegalStateException("Cannot set field " + name + " from object " + object, e);
        } catch (IllegalArgumentException e) {
            throw new IllegalStateException("Cannot set field " + name + " from object " + object, e);
        } catch (IllegalAccessException e) {
            throw new IllegalStateException("Cannot set field " + name + " from object " + object, e);
        }
    }

    /**
     * Get the value of static field {@code name} from class {@code klass}.
     * 
     * @param <V>
     * @param klass
     * @param name
     * @return the value of the static field {@code name} from class
     *         {@code klass}.
     * 
     * @throws IllegalStateException
     *             if cannot get the static field {@code name} from class
     *             {@code klass}.
     * 
     * @see Class#getDeclaredField(String)
     * @see Field#get(Object)
     * @see Field#setAccessible(boolean)
     * @see Field#getModifiers()
     * @see Modifier
     */
    public static Object getStaticField(Class<?> klass, String name) {
        try {
            Field field = klass.getDeclaredField(name);
            field.setAccessible(true);
            // remove final modifier from field
            int modifier = unfinalize(field);
            Object value = field.get(null);
            setModifier(field, modifier);
            field.setAccessible(false);
            return value;
        } catch (SecurityException e) {
            throw new IllegalStateException("Cannot get static field " + name + " from class " + klass, e);
        } catch (NoSuchFieldException e) {
            throw new IllegalStateException("Cannot get static field " + name + " from class " + klass, e);
        } catch (IllegalArgumentException e) {
            throw new IllegalStateException("Cannot get static field " + name + " from class " + klass, e);
        } catch (IllegalAccessException e) {
            throw new IllegalStateException("Cannot get static field " + name + " from class " + klass, e);
        }
    }

    /**
     * 
     * Set static field {@code name} of {@code klass} with {@code value}.
     * 
     * @param <V>
     * @param klass
     * @param name
     * @param value
     * 
     * @throws IllegalStateException
     *             if cannot set the specified static field of the specified
     *             class with the value.
     * 
     * @see Class#getDeclaredField(String)
     * @see Field#set(Object, Object)
     * @see Field#setAccessible(boolean)
     * @see Field#getModifiers()
     * @see Modifier
     */
    public static <V> void setStaticField(Class<?> klass, String name, V value) {
        try {
            Field field = klass.getDeclaredField(name);
            field.setAccessible(true);
            // remove final modifier from field
            int modifier = unfinalize(field);
            field.set(null, value);
            setModifier(field, modifier);
            field.setAccessible(false);
        } catch (SecurityException e) {
            throw new IllegalStateException("Cannot set static field " + name + " from class " + klass, e);
        } catch (NoSuchFieldException e) {
            throw new IllegalStateException("Cannot set static field " + name + " from class " + klass, e);
        } catch (IllegalArgumentException e) {
            throw new IllegalStateException("Cannot set static field " + name + " from class " + klass, e);
        } catch (IllegalAccessException e) {
            throw new IllegalStateException("Cannot set static field " + name + " from class " + klass, e);
        }
    }

    private static int unfinalize(Field field) {
        try {
            Field modifiersField = Field.class.getDeclaredField("modifiers");
            modifiersField.setAccessible(true);
            int modifier = field.getModifiers();
            if (Modifier.isFinal(modifier)) {
                modifiersField.setInt(field, modifier & ~Modifier.FINAL);
            }
            modifiersField.setAccessible(false);
            return modifier;
        } catch (SecurityException e) {
            throw new IllegalStateException("Cannot change modifier for field " + field, e);
        } catch (NoSuchFieldException e) {
            throw new IllegalStateException("Cannot change modifier for field " + field, e);
        } catch (IllegalArgumentException e) {
            throw new IllegalStateException("Cannot change modifier for field " + field, e);
        } catch (IllegalAccessException e) {
            throw new IllegalStateException("Cannot change modifier for field " + field, e);
        }
    }

    private static void setModifier(Field field, int modifier) {
        try {
            Field modifiersField = Field.class.getDeclaredField("modifiers");
            modifiersField.setAccessible(true);
            modifiersField.setInt(field, modifier);
            modifiersField.setAccessible(false);
        } catch (SecurityException e) {
            throw new IllegalStateException("Cannot change modifier for field " + field, e);
        } catch (NoSuchFieldException e) {
            throw new IllegalStateException("Cannot change modifier for field " + field, e);
        } catch (IllegalArgumentException e) {
            throw new IllegalStateException("Cannot change modifier for field " + field, e);
        } catch (IllegalAccessException e) {
            throw new IllegalStateException("Cannot change modifier for field " + field, e);
        }
    }

}
