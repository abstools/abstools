package org.abs_models.backend.java.lib;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.*;
import java.util.stream.Collectors;

/**
 * A hashmap with weak values.
 */
public class WeakValueHashMap<K, V> implements Map {
    // See
    // https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/WeakHashMap.java
    // but we implement weak values instead of weak keys, and
    // implement things by wrapping another hash table since that is
    // easier to read.

    private static final class ValueEntry<K, V> extends WeakReference<V> {
        // We store the key in addition to the value so that `expunge`
        // can remove the map entry when `value` gets
        // garbage-collected
        final K key;
        ValueEntry(K key, V value, ReferenceQueue<V> queue) {
            super(value, queue);
            this.key = key;
        }
    }

    private final Map<K, ValueEntry<K, V>> map = new HashMap<>();
    private final ReferenceQueue<V> queue = new ReferenceQueue<>();

    private void expunge() {
        ValueEntry<K, V> entry;
        while ((entry = (ValueEntry<K, V>) queue.poll()) != null) {
            map.remove(entry.key);
        }
    }

    public V put(Object key, Object value) {
        expunge();
        V result = get(key);
        map.put((K)key, new ValueEntry<>((K)key, (V)value, queue));
        return result;
    }

    public void putAll(Map m) {
        expunge();
        m.forEach((k, v) -> map.put((K)k, new ValueEntry<>((K)k, (V)v, queue)));
    }

    public V get(Object key) {
        expunge();
        ValueEntry<K, V> entry = map.get(key);
        return entry == null ? null : entry.get();
    }

    public V remove(Object key) {
        expunge();
        V result = get(key);
        map.remove(key);
        return result;
    }

    public Set<K> keySet() {
        expunge();
        return map.keySet();
    }

    public Set<Map.Entry<K, V>> entrySet() {
        expunge();
        return map.entrySet().stream()
            .map(Map.Entry::getValue)
            .filter(e -> e != null)
            .map(e -> new AbstractMap.SimpleImmutableEntry<K, V>(e.key, e.get()))
            .collect(Collectors.toUnmodifiableSet());
    }

    public Collection<V> values() {
        expunge();
        return map.entrySet().stream()
            .map(Map.Entry::getValue)
            .filter(e -> e != null)
            .map(e -> e.get())
            .toList();
    }

    public int size() {
        expunge();
        return map.size();
    }

    public boolean isEmpty() {
        expunge();
        return map.isEmpty();
    }

    public void clear() {
        expunge(); // clear the queue just in case; not optimizing this for now
        map.clear();
    }

    public boolean containsKey(Object key) {
        expunge();
        return map.containsKey(key);
    }

    public boolean containsValue(Object value) {
        return values().contains(value);
    }
}
