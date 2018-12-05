package ch.isageek.ads.p8;

import org.junit.Test;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static java.util.Arrays.asList;

public class SortComparison {

    private static final long SEED = 102300303923L;
    private static final int NUMBER_OF_ELEMENTS = 10_000_000;
    private static final int RANDOM_REPEATS = 10;
    private Random randomGenerator = new Random(SEED);

    private static final Collection<Integer> SIZES_INSERTION = IntStream.range(0, 7).map(i -> (int) Math.pow(10, i)).boxed().collect(Collectors.toList());
    private static final Collection<Integer> SIZES_QUICKSORT = IntStream.range(0, 9).map(i -> (int) Math.pow(10, i)).boxed().collect(Collectors.toList());

//    private static final Collection<Integer> CUTOFFS = IntStream.range(80, 120).boxed().collect(Collectors.toList());
    private static final Collection<Integer> CUTOFFS = asList(76, 77, 72, 54, 60, 52, 74, 78, 69, 85, 51, 66, 75, 53, 65, 55, 70, 86, 71, 79, 87, 105, 64, 49);


    @Test
    public void compareInsertionSortWithClassic() {
        System.out.println("Comparing InsertionSort to QuicksortClassic");
        QuickSortBase classic = new QuickSortClassic();
        InsertionSort insertion = new InsertionSort();
        System.out.println("Length\tQuicksort(ms)\tInsertionsort(ms)");
        testRuntimes(classic, insertion, SIZES_INSERTION);
    }

    @Test
    public void compareMedianWithClassic() {
        System.out.println("Comparing QuicksortClassic to QuicksortMedian");
        QuickSortBase classic = new QuickSortClassic();
        QuicksortMedian median = new QuicksortMedian();
        System.out.println("Length\tQuicksortClassic(ms)\tQuicksortMedian(ms)");
        testRuntimes(classic, median, SIZES_QUICKSORT);
    }

    private void testRuntimes(Sorter a, Sorter b, Collection<Integer> sizes) {
        for (Integer size : sizes) {
            long startA = System.currentTimeMillis();
            for (int i = 0; i < 5; i++) {
                sortAscending(a, size);
                sortDescending(a, size);
                sortRandom(a, size);
            }
            long durationA = System.currentTimeMillis() - startA;

            long startB = System.currentTimeMillis();
            for (int i = 0; i < 5; i++) {
                sortAscending(b, size);
                sortDescending(b, size);
                sortRandom(b, size);
            }
            long durationB = System.currentTimeMillis() - startB;
            System.out.println(String.format("%d\t%d\t%d", size, durationA, durationB));
        }
    }


    @Test
    public void compareCutoffs() {
        System.out.println(String.format("Sorting %d elements %d times in ascending, descending and random order.", NUMBER_OF_ELEMENTS, RANDOM_REPEATS));
        List<CutoffRun> runtimes = new ArrayList<>(CUTOFFS.size());
        for (Integer cutoff : CUTOFFS) {
            // Reset generator so we use the same arrays for every cutoff
            randomGenerator = new Random(SEED);
            QuicksortTurbo sorter = new QuicksortTurbo(cutoff);
            long start = System.currentTimeMillis();
            for (int i = 0; i < RANDOM_REPEATS; i++) {
                sortAscending(sorter, NUMBER_OF_ELEMENTS);
                sortDescending(sorter, NUMBER_OF_ELEMENTS);
                sortRandom(sorter, NUMBER_OF_ELEMENTS);
            }

            long end = System.currentTimeMillis();
            System.out.println(String.format("%d\t%d", cutoff, end - start));
            runtimes.add(new CutoffRun(end - start, cutoff));
        }

        runtimes.sort(Comparator.comparingLong(o -> o.duration));
        System.out.println("Runtimes top 20%");
        for (int i = 0; i < 0.2 * runtimes.size(); i++) {
            System.out.println(runtimes.get(i));
        }
    }

    public void sortAscending(Sorter sorter, int size) {
        int[] array = ascending(size);
        sorter.sort(array);
    }

    public void sortRandom(Sorter sorter, int size) {
        int[] array = random(size);
        sorter.sort(array);
    }

    public void sortDescending(Sorter sorter, int size) {
        int[] array = descending(size);
        sorter.sort(array);
    }


    private int[] ascending(int size) {
        return IntStream.range(0, size).toArray();
    }

    private int[] descending(int size) {
        return IntStream.range(0, size).map(i -> size - i - 1).toArray();
    }

    private int[] random(int size) {
        return randomGenerator.ints(size).toArray();
    }

    private static class CutoffRun {
        public CutoffRun(long duration, int cutoff) {
            this.duration = duration;
            this.cutoff = cutoff;
        }

        public long duration;
        public int cutoff;

        @Override
        public String toString() {
            return String.format("%d\t%d", cutoff, duration);
        }
    }
}
