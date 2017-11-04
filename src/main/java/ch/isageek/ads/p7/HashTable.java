package ch.isageek.ads.p7;

import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Iterator;
import java.util.stream.Stream;

public interface HashTable<T> {

    /**
     * Returns number of elements in the hashtable
     * @return the number of elements in the hashtable
     */
    int size();

    /**
     * Returns true if the table is empty
     * @return true if the table is empty, false otherwise
     */
    boolean isEmpty();

    /**
     * Adds an element to the table
     * @param element The object to add
     */
    void add(@NotNull T element);

    /**
     * Tests whether the element exists in the table
     * @param element The element to find
     * @return true if the hashtable contains the element, false otherwise
     */
    boolean contains(@NotNull T element);

    /**
     * Removes the given element from the hashtable
     * @param element the element to remove
     * @return true if the element was in the table, false otherwise
     */
    boolean remove(@NotNull T element);

    /**
     * Set the loadfactor which the table uses to grow
     * @param loadfactor the loadfactor, should be between 0.01 and 1 (values above will be 1, values below will be 0.01)
     */
    void setLoadFactorForResize(float loadfactor);

    /**
     * Returns an {@link Iterator} over the array backing the hashtable
     * Empty buckets (either from removing an element or never adding one there) will be returned as <b>null</b>
     * The {@link Iterator} iterates over the elements in the order they are stored.
     * @return An {@link Iterator} over all elements in the hashtable
     */
    Iterator<T> iterator();

    /**
     * Returns a {@link Stream} of  all elements in the table
     * Empty buckets (either from removing an element or never adding one there) will be returned as <b>null</b>
     * The {@link Stream} contains the elements in the order they are stored
     * @return a {@link Stream} of all elements
     */
    Stream<T> stream();

    /**
     * Add all elements of the given {@link Collection} to the hashtable
     * @param elements the {@link Collection} of elements to add
     */
    void addAll(@NotNull Collection<T> elements);
}
