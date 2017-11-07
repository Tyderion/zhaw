package ch.isageek.ads.p8;

import java.util.Arrays;
import java.util.stream.Collectors;

public class Quicksort implements Sorter {
    @Override
    public void sort(int[] numbers) {
        if (numbers == null || numbers.length == 1) {
            return;
        }
        quicksort(numbers, 0, numbers.length -1);
    }

    protected void quicksort(int[] numbers, int low, int high) {
        System.out.println(String.format("Quicksort from %d to %d", low, high));
        if (low < high) {
            int pivotIndex = (low + high) / 2;
            int partitionIndex = partition(numbers, low, high, pivotIndex);
            quicksort(numbers, low, partitionIndex - 1);
            quicksort(numbers, partitionIndex + 1, high);
        }
    }

    protected int partition(int[] numbers, int low, int high, int pivotIndex) {
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
            while (numbers[right] > pivot|| (right >  pivotIndex && numbers[right] >= pivot) ) {
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

    protected void swap(int[] numbers, int left, int right) {
        int temp = numbers[left];
        numbers[left] = numbers[right];
        numbers[right] = temp;
    }
}
