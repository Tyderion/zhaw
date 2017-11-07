package ch.isageek.ads.p8;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class Quicksort implements Sorter {
    @Override
    public void sort(int[] numbers) {
        if (numbers == null) {
            return;
        }
        quicksort(numbers, 0, numbers.length - 1);
    }


    protected int getPivot(int[] numbers, int low, int high) {
        return (low + high) / 2;
    }

    private void quicksort(int[] numbers, int low, int high) {
        if (low >= high) {
            return;
        }
        int partitionIndex = partition(numbers, low, high, getPivot(numbers, low, high));
        quicksort(numbers, low, partitionIndex - 1);
        quicksort(numbers, partitionIndex, high);
    }


    int partition(int arr[], int left, int right, int pivotIndex) {
        int i = left, j = right;
        int pivot = arr[pivotIndex];

        while (i <= j) {
            while (arr[i] < pivot)
                i++;
            while (arr[j] > pivot)
                j--;
            if (i <= j) {
                swap(arr, i, j);
                i++;
                j--;
            }
        }
        ;

        return i;
    }


    private void swap(int[] numbers, int left, int right) {
        int temp = numbers[left];
        numbers[left] = numbers[right];
        numbers[right] = temp;
    }
}
