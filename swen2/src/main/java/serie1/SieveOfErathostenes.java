package serie1;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * This class Generates prime numbers up to a user specified maximum.
 * the algorithm used is the Sieve of Eratosthenes.
 * <p>
 * Eratosthenes of Cyrene, b. c. 276 BC, Cyrene, Libya --
 * d. c. 194, Alexandria. The first man to calculate the circumference
 * of the Earth. Also known for working on calendars with leap
 * years and ran the library at Alexandria.
 * <p>
 * The algorithm is quite simple. Given an array of integers starting
 * at 2. Cross out all multiples of 2. Find the next uncrossed
 * integer, and cross out all of its multiples. Repeat until
 * you have passed the square root of the maximum value.
 *
 * @author Your Name
 * @version 1.0
 */
public class SieveOfErathostenes {
    public static int[] generatePrimes(int maxValue) {
        if (maxValue < 2) {
            throw new IllegalArgumentException("maxValue must be > 2");
        }
        int[] primeCandiates = IntStream.rangeClosed(2, maxValue).toArray();

        int lastIndex = 0;
        final int limit = (int) Math.sqrt(maxValue);
        while (lastIndex < limit) {
            final int current = primeCandiates[lastIndex];
            crossOutMultiplesStartingAt(primeCandiates, current, lastIndex);
            lastIndex = findNextUncrossedValue(primeCandiates, lastIndex + 1);
        }

        return Arrays.stream(primeCandiates).filter(i -> i > 0).toArray();
    }

    private static int findNextUncrossedValue(int[] array, int startingIndex) {
        for (; startingIndex < array.length -1; startingIndex++) {
            if (array[startingIndex] != -1) {
                break;
            }
        }
        return startingIndex;
    }

    private static void crossOutMultiplesStartingAt(int[] array, int value, int startingIndex) {
        for (int i = startingIndex + value; i <= array.length - 1; i += value) {
            if (array[i] % value == 0) {
                array[i] = -1;
            }
        }
    }
}
