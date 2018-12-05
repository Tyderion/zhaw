package ch.isageek.ads.p2;

interface GenericStackExercise<T> {
    public void push(T x);

    public T pop();

    public T top();

    public boolean isEmpty();
}

public class GenericStack<T> implements GenericStackExercise<T> {

    private static final int MIN_SIZE = 10;
    private Object[] store;
    private int index;

    public GenericStack() {
        index = 0;
        store = new Object[MIN_SIZE];
    }

    @Override
    public void push(T x) {
        growIfNeeded();
        store[index++] = x;
    }

    @Override
    @SuppressWarnings("unchecked")
    public T pop() {
        // This uncheck cast will always work because we have CompileTime TypeChecks for the generics.
        // ArrayList#get is implemented very similarly.
        T ele = (T) store[--index];
        shrinkIfNeeded();
        return ele;
    }

    @Override
    @SuppressWarnings("unchecked")
    public T top() {
        // This uncheck cast will always work because we have CompileTime TypeChecks for the generics.
        // ArrayList#get is implemented very similarly.
        return (T) store[index - 1];
    }

    @Override
    public boolean isEmpty() {
        return index == 0;
    }

    /**
     * Grow the array to double the size if the current index is out of bounds.
     */
    /**
     * Grow the array to double the size if the current index is out of bounds.
     */
    private void growIfNeeded() {
        if (index >= store.length) {
            Object[] newStore = new Object[store.length * 2];
            System.arraycopy(store, 0, newStore, 0, store.length);
            store = newStore;
        }
    }

    /**
     * Shrink the array to half the size if the index is in the first third
     */
    private void shrinkIfNeeded() {
        int newLength = store.length / 2;
        if (index < store.length / 3 && newLength >= MIN_SIZE) {
            Object[] newStore = new Object[store.length / 2];
            System.arraycopy(store, 0, newStore, 0, newStore.length);
            store = newStore;
        }
    }
}
