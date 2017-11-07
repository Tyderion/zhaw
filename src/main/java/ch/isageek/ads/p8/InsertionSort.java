package ch.isageek.ads.p8;

public class InsertionSort implements Sorter {
    @Override
    public void sort(int[] numbers) {
        if (numbers == null) {
            return;
        }
        sort(numbers, 0, numbers.length - 1);
    }

    @Override
    public void sort(int[] numbers, int low, int high) {
        if (numbers == null || high - low <= 1) {
            return;
        }
        int i = low + 1;
        while (i <= high) {
            int ele = numbers[i];
            int j = i - 1;
            while (j >= 0 && numbers[j] > ele) {
                numbers[j + 1] = numbers[j];
                j = j - 1;
            }
            numbers[j + 1] = ele;
            i++;
        }
    }
}
