package ch.isageek.ads.p8;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Quicksort implements Sorter {
    @Override
    public void sort(int[] numbers) {
        if (numbers == null || numbers.length == 1) {
            return;
        }
        quicksort(numbers, 0, numbers.length -1);
    }

    private void quicksort(int[] numbers, int low, int high) {
        if (low < high) {
            System.out.println(String.format("Sort from %d to %d", low, high));
            int pivotIndex = getPivot(numbers, low, high);
            System.out.println(String.format("%s: Pivot at %d[%d]", Arrays.toString(numbers), pivotIndex, numbers[pivotIndex]));
            int partitionIndex = partition2(numbers, low, high, pivotIndex);
            System.out.println(String.format("Paritioned: %s", Arrays.toString(numbers)));
            quicksort(numbers, low, partitionIndex - 1);
            quicksort(numbers, partitionIndex + 1, high);
        }
    }

    protected int getPivot(int[] numbers, int low, int high) {
        return (low + high) / 2;
    }
    private int stupidParitioning(int[] numbers, int low, int high, int pivotIndex) {
        // Very inefficient and space consuming partitioning
        int pivot = numbers[pivotIndex];
        List<Integer> smaller = new ArrayList<>();
        List<Integer> bigger = new ArrayList<>();

        for (int i = low; i <= high; i++) {
            if (i == pivotIndex) {
                continue;
            }
            if (numbers[i] <= pivot) {
                smaller.add(numbers[i]);
            }

            if (numbers[i] > pivot) {
                bigger.add(numbers[i]);
            }
        }
        int count = 0;
        for (int i = 0; i < smaller.size(); i++) {
            numbers[low + count] = smaller.get(i);
            count++;
        }

        numbers[low + count] = pivot;
        count++;

        for (int i = 0; i < bigger.size(); i++) {
            numbers[low + count] = bigger.get(i);
            count++;
        }
        int pivotLocation = low + smaller.size();
        return pivotLocation;
    }

    private int partition(int[] numbers, int low, int high, int pivotIndex) {
        int pivot = numbers[pivotIndex];
        System.out.println("---------------------------------------------------------------");
        System.out.println(String.format("Sorting %d to %d with pivot %d: %d", low, high, pivotIndex, pivot));
        System.out.println(String.format("Before: %s", Arrays.toString(numbers)));
        int left = low;
        int right = high;
        boolean leftIsPivot = false;
        boolean rightIsPivot = false;
        while (left < right) {
            while (numbers[left] < pivot || (left < pivotIndex && numbers[left] <= pivot)) {
                // This is for handling the pivot with duplicate elements
                leftIsPivot = left < pivotIndex && numbers[left] <= pivot;
                left++;
            }
            while (numbers[right] > pivot|| (right >  pivotIndex && numbers[right] >= pivot && (left < right-1)) ) {
                // This is for handling the pivot with duplicate elements
                rightIsPivot = right >  pivotIndex && numbers[right] >= pivot;
                right--;
            }
            System.out.println(String.format("(l,r) = (%d, %d)", left, right));
            if (numbers[left] > numbers[right] && left < right) {
                System.out.println(String.format("Swapping %d [%d] with %d [%d]", left, numbers[left], right, numbers[right]));
                swap(numbers, left, right);
                System.out.println(String.format("Swapped: %s", Arrays.toString(numbers)));
            }
        }
        System.out.println(String.format("LeftIs: %b, RightIs: %b", leftIsPivot, rightIsPivot));
        int pivotLocation = rightIsPivot ? left :  left-1;
        System.out.println(String.format("After: %s. Pivot at: %d", Arrays.toString(numbers), pivotLocation));
        System.out.println("---------------------------------------------------------------");
        return pivotLocation;
    }

    private void swap(int[] numbers, int left, int right) {
        int temp = numbers[left];
        numbers[left] = numbers[right];
        numbers[right] = temp;
    }
}
