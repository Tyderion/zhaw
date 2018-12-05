package ch.isageek.ads.p8;

import org.junit.Test;

import static junit.framework.TestCase.assertEquals;

public class MedianTest {

    QuicksortMedian median = new QuicksortMedian();

    @Test
    public void shouldGetMiddleValueMiddle() {
        int[] array = new int[]{1,2,3};

        int pivot = median.getPivot(array, 0, 2);

        assertEquals(1, pivot);
    }

    @Test
    public void shouldGetMiddleValueLast() {
        int[] array = new int[]{1, 3, 2};

        int pivot = median.getPivot(array, 0, 2);

        assertEquals(2, pivot);
    }

    @Test
    public void shouldGetMiddleValueFirst() {
        int[] array = new int[]{2, 3, 1};

        int pivot = median.getPivot(array, 0, 2);

        assertEquals(0, pivot);
    }
}
