package ch.isageek.ads.p2;

interface StackExercise{
	public void push(int x);
	public int pop();
	public int top();
	public boolean isEmpty();
}
public class IntStack implements StackExercise{

    private int[] store;
    private int index;
    private static final int MIN_SIZE = 10;

	public IntStack() {
	    index = 0;
	    store = new int[MIN_SIZE];
	}

	@Override
	public void push(int x) {
        growIfNeeded();
	    store[index++] = x;
	}

	@Override
	public int pop() {
		int ele = store[--index];
		shrinkIfNeeded();
		return ele;
	}

	@Override
	public int top() {
		return store[index-1];
	}

	@Override
	public boolean isEmpty() {
		return index == 0;
	}

    /**
     * Grow the array to double the size if the current index is out of bounds.
     */
    private void growIfNeeded() {
        if (index >= store.length) {
            int[] newStore = new int[store.length * 2];
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
            int[] newStore = new int[store.length / 2];
            System.arraycopy(store, 0, newStore, 0, newStore.length);
            store = newStore;
        }
    }
}
