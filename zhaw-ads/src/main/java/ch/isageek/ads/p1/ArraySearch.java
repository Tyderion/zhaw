package ch.isageek.ads.p1;

public class ArraySearch {
    public static final int KEY_NOT_FOUND = -1;

    public static int sequentialSearch(int[] array, int key) {
        for (int i = 0; i < array.length; i++) {
            if (array[i] == key) {
                return i;
            }
        }
        return KEY_NOT_FOUND;
    }

    public static int binarySearch(int[] array, int key) {
        int start = 0, end = array.length - 1;

        while (start <= end) {
            int index = (end + start) / 2;
            if (array[index] == key) {
                return index;
            } else if (array[index] < key) {
                start = index+1;
            } else {
                end = index-1;
            }
        }
        return KEY_NOT_FOUND;
    }
}
