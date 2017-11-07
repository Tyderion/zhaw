package ch.isageek.ads.p8;

public class InsertionSort implements Sorter {
    @Override
    public void sort(int[] numbers) {
        if (numbers == null || numbers.length == 1) {
            return;
        }
        int i = 1;
        while (i < numbers.length) {
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
