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
        if (!insertAt(element, index)) {
            for (int current = 0; current < table.length; current ++) {
                index = nextIndex(index, current);
                if (insertAt(element, index)) {
                    return;
                }
            }
            // TODO: Grow and insert element because we did not insert it yet
        }
    }

    private boolean insertAt(T element,final int index) {
        final int idx = index % table.length;
        if (table[idx] == null || table[idx].value == null) {
            table[idx] = new Element(element);
            return true;
        }
        return false;
    }

    @Override
    public boolean contains(@NotNull T element) {
        final int index = this.index(element);
        return find(element, index);
    }

    private boolean find(T element, int idx) {
        int count = 0;
        while(table[idx] != null && !element.equals(table[idx].value)) {
            idx = nextIndex(idx, count);
            if (count >= table.length) {
                // If the table is full we won't end the loop at an empty element
                return false;
            }
            count++;
        } ;
        return table[idx] != null && element.equals(table[idx].value);
    }

    private int nextIndex(int current, int iteration) {
        switch (probingMode) {
            case QUADRATIC:
                return QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.get(iteration % QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.size()) % table.length;
            case LINEAR:
                return ++current % table.length;
        }
        return current+1;
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
