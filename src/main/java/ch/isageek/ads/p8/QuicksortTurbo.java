package ch.isageek.ads.p8;

public class QuicksortTurbo extends QuicksortMedian {
    private int cutoff;

    private InsertionSort insertionSort = new InsertionSort();

    public QuicksortTurbo() {
        this(15);
    }

    public QuicksortTurbo(int cutoff) {
        this.cutoff = cutoff;
    }

    @Override
    protected void quicksort(int[] numbers, int low, int high) {
        if (low >= high) {
            return;
        }
        if (high - low <= cutoff) {
            insertionSort.sort(numbers, low, high);
        } else {
            int partitionIndex = partition(numbers, low, high, getPivot(numbers, low, high));
            quicksort(numbers, low, partitionIndex - 1);
            quicksort(numbers, partitionIndex, high);
        }
    }
}
