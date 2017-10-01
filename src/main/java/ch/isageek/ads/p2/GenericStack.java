package ch.isageek.ads.p2;
interface GenericStackExercise<T>{
	public void push(T x);
	public T pop();
	public T top();
	public boolean isEmpty();
}
public class GenericStack<T> implements GenericStackExercise<T> {

    private Object[] store;
    private int index;

	public GenericStack() {
	    index = 0;
	    store = new Object[10];
	}

	@Override
	public void push(T x) {
	    if (index > store.length) {
	        grow();
        }
	    store[index++] = x;
	}

	@Override
	public T pop() {
		return (T) store[--index];
	}

	@Override
	public T top() {
		return (T) store[index-1];
	}

	@Override
	public boolean isEmpty() {
		return index == 0;
	}

	private void grow() {
	    Object[] newStore = new Object[store.length * 2];
        System.arraycopy(store, 0, newStore, 0, store.length);
        store = newStore;
    }
}
