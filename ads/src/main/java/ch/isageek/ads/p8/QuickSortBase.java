package ch.isageek.ads.p8;

public abstract class QuickSortBase implements Sorter {
    @Override
    public void sort(int[] numbers) {
        if (numbers == null) {
            return;
        }
        sort(numbers, 0, numbers.length - 1);
    }

    @Override
    public void sort(int[] numbers, int low, int high) {
        if (numbers == null) {
            return;
        }
        quicksort(numbers, low, high);
    }

    protected abstract int getPivot(int[] numbers, int low, int high);

    protected void quicksort(int[] numbers, int low, int high) {
        if (low >= high) {
            return;
        }
        int partitionIndex = partition(numbers, low, high, getPivot(numbers, low, high));
        quicksort(numbers, low, partitionIndex - 1);
        quicksort(numbers, partitionIndex, high);
    }

    // Done differently than in book, I couldn't make it work with very big arrays with the other way
    int partition(int numbers[], int left, int right, int pivotIndex) {
        int i = left, j = right;
        int pivot = numbers[pivotIndex];

        while (i <= j) {
            while (numbers[i] < pivot) {
                i++;
            }
            while (numbers[j] > pivot) {
                j--;
            }
            if (i <= j) {
                swap(numbers, i, j);
                i++;
                j--;
            }
        }

        return i;
    }


    private void swap(int[] numbers, int left, int right) {
        int temp = numbers[left];
        numbers[left] = numbers[right];
        numbers[right] = temp;
    }
}
