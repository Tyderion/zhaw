package ch.isageek.ads.p1;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

public class ArraySearchComparisonTest {
    private static int[] SIZES = {100, 1000, 10000, 100000, 500000, 1000000, 5000000, 10000000, 50000000, 100000000, 500000000, 1000000000};


    @Test
    public void doComparison() throws InterruptedException {
        List<Result> results = new ArrayList<>(SIZES.length);

        final int key = 500000;
        boolean first = true;
        System.out.println("Size\tSequential\tBinary");
        for (int size : SIZES) {
            final Result result = new Result(size);
            int[] input = Util.generateArrayWithIntegers(size, 50000);
            result.durationBinary = testSize(() -> ArraySearch.binarySearch(input, key));
            result.durationSequential = testSize(() -> ArraySearch.sequentialSearch(input, key));
            results.add(result);
            // The first time the test shows very high numbers probably because of java VM startup issues.
            if (!first) {
                System.out.println(result.toString());
            }
            first = false;
        }
    }

    private long testSize(Execute operation) {
        long startTime = System.nanoTime();
        operation.execute();
        long endTime = System.nanoTime();
        return endTime - startTime;
    }

    @FunctionalInterface
    private static interface Execute {
        void execute();
    }

    private static class Result {
        Result(int count) {
            this.itemCount = count;
        }

        long durationBinary;
        public long durationSequential;
        int itemCount;

        @Override
        public String toString() {
            String sepS = itemCount < 1000 ? "\t\t\t" : itemCount > 5000000 ? "\t" : "\t\t";
            String sepC = durationSequential < 1000 ? "\t\t\t" : durationSequential > 10000000 ? "\t" : "\t\t";
            sepC = "\t";
            sepS = "\t";
            return String.format("%d%s%d%s%d", itemCount, sepS, durationSequential, sepC, durationBinary);
        }
    }
}
