package ch.isageek.ads.p12;

import java.util.NoSuchElementException;

public class Fixpoint {

    /**
     * Finds the Index in the Array numbers where numbers[i] = i
     *
     * @return The index of the element that has the value of that index.
     * @throws NoSuchElementException when there is no element which is the same as its index
     */
    public static int getFixpoint(final int[] numbers) throws NoSuchElementException {
        return getFixpoint(numbers, 0, numbers.length - 1);
    }

    /**
     * Recursively computes the fixpoint by using a divide-and-conquer strategy
     * Each time it gets called, one of the following three things happen:
     * - The search ends because there exists no such Element -> NoSuchElementException
     * - The search ends because the element has been found -> return the index
     * - The search has not yet concluded -> search space gets cut in half, either continuing in the lower or upper half.
     * That means, the search space after is either lower...(lower+upper/2 - 1) or (lower+upper/2 + 1)...upper
     * <p>
     * This means that the Algorithm completes in O(log n) time where n is the number of elements.
     *
     * @param numbers The array to search through
     * @param lower   the lower limit of the search space
     * @param upper   the upper limit of the search space
     * @return The index of the element which has the same value
     * @throws NoSuchElementException if no such element exists
     */
    private static int getFixpoint(final int[] numbers, final int lower, final int upper) throws NoSuchElementException {
        if (lower > upper) {
            throw new NoSuchElementException("No Fixpoint exists");
        }
        int pivotIndex = (lower + upper) / 2;
        int pivotElement = numbers[pivotIndex];
        if (pivotElement == pivotIndex) {
            return pivotIndex;
        } else if (pivotElement < pivotIndex) {
            return getFixpoint(numbers, pivotIndex + 1, upper);
        } else {
            return getFixpoint(numbers, lower, pivotIndex - 1);
        }
    }
}
