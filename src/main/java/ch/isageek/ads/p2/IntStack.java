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

	public IntStack() {
	    index = 0;
	    store = new int[10];
	}

	@Override
	public void push(int x) {
	    if (index > store.length) {
	        grow();
        }
	    store[index++] = x;
	}

	@Override
	public int pop() {
		return store[--index];
	}

	@Override
	public int top() {
		return store[index-1];
	}

	@Override
	public boolean isEmpty() {
		return index == 0;
	}

	private void grow() {
	    int[] newStore = new int[store.length * 2];
        System.arraycopy(store, 0, newStore, 0, store.length);
        store = newStore;
    }
}
