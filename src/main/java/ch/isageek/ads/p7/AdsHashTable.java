package ch.isageek.ads.p7;
import com.sun.istack.internal.NotNull;

import java.util.Collection;
import java.util.Iterator;

public class AdsHashTable<T> implements HashTable<T> {

    private final static int DEFAULT_SIZE = 10;
    private final static ProbingMode DEFAULT_MODE = ProbingMode.LINEAR;

    private final ProbingMode probingMode;
    private Element[] table;

    public AdsHashTable() {
        this(DEFAULT_SIZE, DEFAULT_MODE);
    }

    public AdsHashTable(int initialSize) {
        this(initialSize, DEFAULT_MODE);
    }

    public AdsHashTable(ProbingMode probingMode) {
        this(DEFAULT_SIZE, probingMode);
    }

    public AdsHashTable(int initialSize, ProbingMode probingMode) {
        this.probingMode = probingMode;
        this.table = new Element[initialSize];
    }

    @Override
    public int size() {
        return 0;
    }

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
    public void add(@NotNull T element) {
        int index = index(element);
        if (table[index] == null || table[index].value == null) {
            table[index] = new Element(element);
        } else {
            // TODO: Probing
        }
    }

    @Override
    public boolean contains(@NotNull T element) {
        final int index = index(element);
        return table[index] != null && element.equals(table[index].value);
    }

    @Override
    public boolean remove(@NotNull T element) {
        final int index = index(element);
        if (table[index] != null && element.equals(table[index].value)) {
            table[index].value = null;
            return true;
        }
        return false;
    }

    @Override
    public void setLoadFactorForResize(float loadfactor) {

    }

    @Override
    public Iterator<T> iterator() {
        return null;
    }

    @Override
    public void addAll(Collection<T> elements) {
        elements.forEach(this::add);
    }

    private int index(T element) {
        return element.hashCode() % table.length;
    }

    public static enum ProbingMode {
        LINEAR, QUADRATIC;
    }

    private static class Element {
        Object value;

        public Element(Object value) {
            this.value = value;
        }
    }
}
