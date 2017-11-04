package ch.isageek.ads.p7;


import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class AdsHashTable<T> implements HashTable<T> {

    private final static int DEFAULT_SIZE = 10;
    private final static int GROW_FACTOR = 2;
    private final static float DEFAULT_LOADFACTOR = 0.8f;
    private final static ProbingMode DEFAULT_MODE = ProbingMode.LINEAR;

    private final ProbingMode probingMode;
    private Element<T>[] table;
    private float loadFactorForResize;

    private Set<Integer> probed = new HashSet<>();

    public AdsHashTable() {
        this(DEFAULT_SIZE, DEFAULT_MODE);
    }

    public AdsHashTable(int initialSize, ProbingMode probingMode) {
        this.probingMode = probingMode;
        this.allocateTable(initialSize == 0 ? 1 : initialSize);
        this.loadFactorForResize = DEFAULT_LOADFACTOR;
    }

    @SuppressWarnings("unchecked")
    private void allocateTable(int size) {
        this.table = new Element[this.probingMode.tableSize(size)];
    }

    public AdsHashTable(int initialSize) {
        this(initialSize, DEFAULT_MODE);
    }

    public AdsHashTable(ProbingMode probingMode) {
        this(DEFAULT_SIZE, probingMode);
    }

    @Override
    public boolean isEmpty() {
        return this.size() == 0;
    }

    @Override
    public int size() {
        return (int) this.stream().filter(Objects::nonNull).count();
    }

    @Override
    public Stream<T> stream() {
        return Arrays.stream(this.table).map(this::unpackElement);
    }

    @Override
    public void add(@NotNull T element) {
        final int originalIndex = this.generateIndex(element);
        int index = originalIndex;
        int count = 0;
        while (!this.insertAt(element, index)) {
            index = this.getNextPossibleIndex(originalIndex, count);
            count++;
        }
        if (this.getCurrentLoad() >= this.loadFactorForResize) {
            this.grow();
        }
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

    private int getNextPossibleIndex(final int original, final int iteration) {
        return Math.floorMod(original + this.probingMode.stepSize(iteration), this.table.length);
    }

    private float getCurrentLoad() {
        return this.size() / (float) this.table.length;
    }

    private void grow() {
        List<T> elements = this.stream().filter(Objects::nonNull).collect(Collectors.toList());
        this.allocateTable(this.table.length * GROW_FACTOR);
        this.addAll(elements);
    }

    @Override
    public void addAll(@NotNull Collection<T> elements) {
        elements.forEach(this::add);
    }

    @Override
    public boolean contains(@NotNull T element) {
        return this.find(element, this.generateIndex(element)) != -1;
    }

    private int find(@NotNull T element, final int originalIndex) {
        int idx = originalIndex;
        for (int count = 0; count < this.table.length; count++) {
            // If table[idx] is null it has never been allocated so no probing ever got to this position
            if (this.table[idx] == null) {
                return -1;
            }
            if (this.table[idx].contains(element)) {
                return idx;
            }
            idx = getNextPossibleIndex(originalIndex, count);
        }
        return -1;
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
        this.loadFactorForResize = loadFactor;
    }

    @Override
    public Iterator<T> iterator() {
        return this.stream().iterator();
    }

    private T unpackElement(Element<T> element) {
        return element == null ? null : element.value;
    }

    public enum ProbingMode {
        LINEAR(i -> i + 1, minSize -> minSize),
        QUADRATIC(
                i -> (i % 2 == 0 ? 1 : -1) * (int) Math.pow(i / 2 + 1, 2),
                minSize -> {
                    for (int i = 0; i < QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.size(); i++) {
                        if (QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.get(i) > minSize) {
                            return QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.get(i);
                        }
                    }
                    throw new RuntimeException(String.format("AdsHashTable only supports up to %d elements", QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.get(QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.size() - 1)));
                });

        private Function<Integer, Integer> step;
        private Function<Integer, Integer> table;

        ProbingMode(Function<Integer, Integer> step, Function<Integer, Integer> table) {
            this.step = step;
            this.table = table;
        }

        private int stepSize(int iteration) {
            return this.step.apply(iteration);
        }

        private int tableSize(int minSize) {
            return this.table.apply(minSize);
        }
    }

    private static class Element<T> {
        T value;

        Element(T value) {
            this.value = value;
        }

        static boolean notEmpty(@Nullable Element ele) {
            return !isEmpty(ele);
        }

        static boolean isEmpty(@Nullable Element ele) {
            return ele == null || ele.value == null;
        }

        boolean contains(@NotNull T value) {
            return value.equals(this.value);
        }
    }
}
