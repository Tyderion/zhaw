package ch.isageek.ads.p7;

import com.sun.istack.internal.NotNull;

import javax.swing.*;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class AdsHashTable<T> implements HashTable<T> {

    private final static int DEFAULT_SIZE = 10;
    private final static ProbingMode DEFAULT_MODE = ProbingMode.LINEAR;

    private final ProbingMode probingMode;
    private Element<T>[] table;
    private float loadFactor;

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
        createTable(initialSize);
        this.loadFactor = 0.8f;
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
        if (currentLoadFactor() >= loadFactor) {
            grow();
        }
        int index = index(element);

        int count = 0;
        while (!insertAt(element, index)) {
            index = probingMode.nextInt(index, count) % table.length;
            count++;
        }
    }

    @Override
    public boolean contains(@NotNull T element) {
        final int index = this.index(element);
        return find(element, index) != -1;
    }

    @Override
    public boolean remove(@NotNull T element) {
        final int index = index(element);
        int found = find(element, index);
        if (found != -1) {
            table[found].value = null;
            return true;
        }
        return false;
    }

    @Override
    public void setLoadFactorForResize(float loadFactor) {
        this.loadFactor = loadFactor;
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

    private float currentLoadFactor() {
        return size() / (float) table.length;
    }

    private void grow() {
        Element<T>[] existing = this.table;
        createTable(this.table.length * 2);

        this.addAll(Arrays.stream(existing).filter(ele -> ele != null && ele.value != null).map(ele -> ele.value).collect(Collectors.toList()));
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

    private int find(T element, int idx) {
        int count = 0;
        while (table[idx] != null && !element.equals(table[idx].value)) {
            idx = nextIndex(idx, count);
            if (count >= table.length) {
                // If the table is full we won't end the loop at an empty element
                return -1;
            }
            count++;
        }

        return table[idx] != null && element.equals(table[idx].value) ? idx : -1;
    }

    private int nextIndex(int current, int iteration) {
        return probingMode.nextInt(current, iteration) % table.length;
    }

    @SuppressWarnings("unchecked")
    private void createTable(int size) {
        this.table = new Element[size];
    }

    public enum ProbingMode {
        LINEAR(i -> i + 1),
        QUADRATIC(i -> QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.get(i % QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.size()));

        private Function<Integer, Integer> generator;

        ProbingMode(Function<Integer, Integer> generator) {
            this.generator = generator;
        }

        private int nextInt(int value, int iteration) {
            return value + this.generator.apply(iteration);
        }
    }

    private static class Element<T> {
        T value;

        public Element(T value) {
            this.value = value;
        }
    }
}
