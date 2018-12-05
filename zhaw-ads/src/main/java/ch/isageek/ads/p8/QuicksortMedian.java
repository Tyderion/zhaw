package ch.isageek.ads.p8;

public class QuicksortMedian extends QuickSortBase {
    @Override
    protected int getPivot(int[] numbers, int low, int high) {
        int middle = (low + high) / 2;
        if (numbers[low] <= numbers[high] && numbers[low] >= numbers[middle] || numbers[low] >= numbers[high] && numbers[low] <= numbers[middle]) {
            return low;
        }

        if (numbers[high] <= numbers[low] && numbers[high] >= numbers[middle] || numbers[high] >= numbers[low] && numbers[high] <= numbers[middle]) {
            return high;
        }
        return middle;
    }
}
