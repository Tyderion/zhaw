package ch.isageek.ads.p8;

public class QuicksortTurbo extends QuicksortMedian {
    private static final int CUTOFF_SIZE = 100;

    private InsertionSort insertionSort = new InsertionSort();

    @Override
    protected void quicksort(int[] numbers, int low, int high) {
        if (low >= high) {
            return;
        }
        if (high - low <= CUTOFF_SIZE) {
            insertionSort.sort(numbers, low, high+1);
        } else {
            int partitionIndex = partition(numbers, low, high, getPivot(numbers, low, high));
            quicksort(numbers, low, partitionIndex - 1);
            quicksort(numbers, partitionIndex, high);
        }
    }
}
