package ch.isageek.ads.p7;


import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
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
        this.allocateTable(initialSize);
        this.loadFactor = 0.8f;
    }

    @Override
    public int size() {
        return (int) this.stream().filter(Objects::nonNull).count();
    }

    @Override
    public boolean isEmpty() {
        return this.size() == 0;
    }

    @Override
    public void add(@NotNull T element) {
        if (this.getCurrentLoad() >= this.loadFactor) {
            this.grow();
        }

        final int originalIndex = this.generateIndex(element);
        int index = originalIndex;
        int count = 0;
        while (!this.insertAt(element, index)) {
            index = this.getNextPossibleIndex(originalIndex, count);
            System.out.println(String.format("Trying to insert at: %d count: %d", index, count));
            count++;
        }
    }

    @Override
    public boolean contains(@NotNull T element) {
        return this.find(element, this.generateIndex(element)) != -1;
    }

    @Override
    public boolean remove(@NotNull T element) {
        int found = this.find(element, this.generateIndex(element));
        if (found != -1) {
            this.table[found].value = null;
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
        return this.stream().iterator();
    }

    @Override
    public Stream<T> stream() {
        return Arrays.stream(this.table).map(this::unpackElement);
    }

    @Override
    public void addAll(@NotNull Collection<T> elements) {
        elements.forEach(this::add);
    }

    private float getCurrentLoad() {
        return this.size() / (float) this.table.length;
    }

    private void grow() {
        List<T> elements = this.stream().filter(Objects::nonNull).collect(Collectors.toList());
        this.allocateTable(this.table.length * 2);
        this.addAll(elements);
    }

    private int generateIndex(@NotNull T element) {
        return element.hashCode() % table.length;
    }

    private boolean insertAt(@NotNull T element, final int index) {
        final int idx = index % this.table.length;
        if (Element.isEmpty(this.table[idx])) {
            this.table[idx] = new Element<>(element);
            return true;
        }
        return false;
    }

    private int find(@NotNull T element, int idx) {
        for (int count = 0; count < this.table.length; count++) {
            // If table[idx] is null it has never been allocated so no probing ever go to this position
            if (this.table[idx] == null) {
                return -1;
            }
            if (this.table[idx].contains(element)) {
                return idx;
            }
            idx = getNextPossibleIndex(idx, count);
        }
        return -1;
    }

    private int getNextPossibleIndex(int current, int iteration) {
        return this.probingMode.nextInt(current, iteration) % this.table.length;
    }

    private T unpackElement(Element<T> element) {
        return element == null ? null : element.value;
    }

    @SuppressWarnings("unchecked")
    private void allocateTable(int size) {
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

        Element(T value) {
            this.value = value;
        }

        boolean contains(@NotNull T value) {
            return value.equals(this.value);
        }

        static boolean isEmpty(@Nullable Element ele) {
            return ele == null || ele.value == null;
        }

        static boolean notEmpty(@Nullable Element ele) {
            return !isEmpty(ele);
        }
    }
}
