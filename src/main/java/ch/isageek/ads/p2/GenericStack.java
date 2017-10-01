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
    @SuppressWarnings("unchecked")
	public T pop() {
	    // This uncheck cast will always work because we have CompileTime TypeChecks for the generics.
        // ArrayList#get is implemented very similarly.
		T ele = (T) store[--index];
		shrink();
		return ele;
	}

	@Override
    @SuppressWarnings("unchecked")
	public T top() {
        // This uncheck cast will always work because we have CompileTime TypeChecks for the generics.
        // ArrayList#get is implemented very similarly.
		return (T) store[index-1];
	}

	@Override
	public boolean isEmpty() {
		return index == 0;
	}

    /**
     * Grow the array to double the size
     */
	private void grow() {
	    Object[] newStore = new Object[store.length * 2];
        System.arraycopy(store, 0, newStore, 0, store.length);
        store = newStore;
    }

    /**
     * Shrink the array to half the size if the index is in the first third
     */
    private void shrink() {
        if (index < store.length / 3) {
            Object[] newStore = new Object[store.length / 2];
            System.arraycopy(store, 0, newStore, 0, newStore.length);
            store = newStore;
        }
    }
}
