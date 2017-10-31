package ch.isageek.ads.p7;

import java.util.Collection;
import java.util.Iterator;

public class AdsHashTable<T> implements HashTable<T> {

    private final static int DEFAULT_SIZE = 10;
    private final static ProbingMode DEFAULT_MODE = ProbingMode.LINEAR;

    private final ProbingMode probingMode;
    private Object[] table;

    public static enum ProbingMode {
        LINEAR, QUADRATIC;
    }

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
        this.table = new Object[initialSize];
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
    public void add(T element) {

    }

    @Override
    public boolean contains(T element) {
        return false;
    }

    @Override
    public boolean remove(T element) {
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
}
