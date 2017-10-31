package ch.isageek.ads.p7;

import com.sun.istack.internal.NotNull;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Objects;
import java.util.stream.Stream;

public class AdsHashTable<T> implements HashTable<T> {

    private final static int DEFAULT_SIZE = 10;
    private final static ProbingMode DEFAULT_MODE = ProbingMode.LINEAR;

    private final ProbingMode probingMode;
    private Element<T>[] table;

    public AdsHashTable() {
        this(DEFAULT_SIZE, DEFAULT_MODE);
    }

    public AdsHashTable(int initialSize) {
        this(initialSize, DEFAULT_MODE);
    }

    public AdsHashTable(ProbingMode probingMode) {
        this(DEFAULT_SIZE, probingMode);
    }

    @SuppressWarnings("unchecked")
    public AdsHashTable(int initialSize, ProbingMode probingMode) {
        this.probingMode = probingMode;
        this.table = new Element[initialSize];
    }

    @Override
    public int size() {
        return (int) stream().filter(Objects::nonNull).count();
    }

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
    public void add(@NotNull T element) {
        int index = index(element);
        if (!insertAt(element, index)) {
            for (int current = 0; current < table.length; current++) {
                index = nextIndex(index, current);
                if (insertAt(element, index)) {
                    return;
                }
            }
            // TODO: Grow and insert element because we did not insert it yet
        }
    }

    @Override
    public boolean contains(@NotNull T element) {
        final int index = this.index(element);
        return find(element, index);
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
        return stream().iterator();
    }

    @Override
    public Stream<T> stream() {
        return Arrays.stream(table).map(ele -> ele != null ? ele.value : null);
    }

    @Override
    public void addAll(Collection<T> elements) {
        elements.forEach(this::add);
    }



    private int index(T element) {
        return element.hashCode() % table.length;
    }

    private boolean insertAt(T element, final int index) {
        final int idx = index % table.length;
        if (table[idx] == null || table[idx].value == null) {
            table[idx] = new Element<>(element);
            return true;
        }
        return false;
    }

    private boolean find(T element, int idx) {
        int count = 0;
        while (table[idx] != null && !element.equals(table[idx].value)) {
            idx = nextIndex(idx, count);
            if (count >= table.length) {
                // If the table is full we won't end the loop at an empty element
                return false;
            }
            count++;
        }
        ;
        return table[idx] != null && element.equals(table[idx].value);
    }

    private int nextIndex(int current, int iteration) {
        switch (probingMode) {
            case QUADRATIC:
                return QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.get(iteration % QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.size()) % table.length;
            case LINEAR:
                return ++current % table.length;
        }
        return current + 1;
    }

    public enum ProbingMode {
        LINEAR, QUADRATIC;
    }

    private static class Element<T> {
        T value;

        public Element(T value) {
            this.value = value;
        }
    }
}
