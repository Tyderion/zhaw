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
    /**
     * According to https://cathyatseneca.gitbooks.io/data-structures-and-algorithms/tables/quadratic_probing_and_double_hashing.html
     * quadratic probing only works correctly when the table is filled less than 50%.
     * If this is written in the text on peruseall, I didn't see it.
     * 0^2, 1^2, 2^2, ..., 6^2 [all % 7] DOES NOT HIT EVERY INDEX! [hits: 0, 1, 4, 2, 2, 4, 1]
     * It's only guaranteed to hit a free cell if the above condition is true.
     */
    private final static float DEFAULT_LOADFACTOR = 0.5f;
    private final static ProbingMode DEFAULT_MODE = ProbingMode.LINEAR;

    private final ProbingMode probingMode;
    private Element<T>[] table;
    private float loadFactorForResize;

    private Set<Integer> probed = new HashSet<>();

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
        this.allocateTable(initialSize == 0 ? 1 : initialSize);
        this.loadFactorForResize = DEFAULT_LOADFACTOR;
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
        final int originalIndex = this.generateIndex(element);
        int index = originalIndex;
        int count = 0;
        while (!this.insertAt(element, index)) {
            index = this.getNextPossibleIndex(originalIndex, count);
            count++;
            if (count > table.length*2) {
              throw new RuntimeException("Probing does not hit all elements!");
            }
        }
        if (this.getCurrentLoad() >= this.loadFactorForResize) {
            this.grow();
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
        if (loadFactor > 0.5f && probingMode == ProbingMode.QUADRATIC) {
            System.out.print("WARNING: Quadratic Probing is only guaranteed to work when the table is less than half full. See: https://cathyatseneca.gitbooks.io/data-structures-and-algorithms/tables/quadratic_probing_and_double_hashing.html");
        }
        this.loadFactorForResize = loadFactor;
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
        this.allocateTable(this.table.length * GROW_FACTOR);
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

    private int getNextPossibleIndex(final int original, final int iteration) {
        return (original + this.probingMode.stepSize(iteration)) % this.table.length;
    }

    private T unpackElement(Element<T> element) {
        return element == null ? null : element.value;
    }

    @SuppressWarnings("unchecked")
    private void allocateTable(int size) {
        this.table = new Element[this.probingMode.tableSize(size)];
    }

    public enum ProbingMode {
        LINEAR(i -> i + 1, minSize -> minSize),
        QUADRATIC(i -> (int)Math.pow(i+1, 2), minSize -> {
            for (int i = 0; i < QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.size(); i++) {
                if (QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.get(i) > minSize) {
                    return QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.get(i);
                }
            }
            throw new RuntimeException(String.format("AdsHashTable only supports up to %d elements", QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.get(QuadraticProbe.QUADRATIC_PROBING_HASH_TABLE_SIZE_LIST.size()-1)));
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
