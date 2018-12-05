package ch.isageek.ads.p12;

import org.junit.Test;

import java.util.NoSuchElementException;

import static junit.framework.TestCase.assertEquals;

public class FixpointTest {

    @Test
    public void testOneElementArray() throws Exception {
        int[] array = new int[]{0};

        int index = Fixpoint.getFixpoint(array);

        assertEquals(0, index);
    }

    @Test
    public void testTwoElementArray() throws Exception {
        int[] array = new int[]{-10, 1};

        int index = Fixpoint.getFixpoint(array);

        assertEquals(1, index);
    }

    @Test
    public void testFourElementArray() throws Exception {
        int[] array = new int[]{-50, -12, -5, 3};

        int index = Fixpoint.getFixpoint(array);

        assertEquals(3, index);
    }

    @Test(expected = NoSuchElementException.class)
    public void testNoFixpoint() throws Exception {
        int[] array = new int[]{-50, -12, -5, 4};
        Fixpoint.getFixpoint(array);
    }

    @Test(expected = NoSuchElementException.class)
    public void testNoFixpointAllLower() throws Exception {
        int[] array = new int[]{-50, -12, -5, -2};
        Fixpoint.getFixpoint(array);
    }

    @Test(expected = NoSuchElementException.class)
    public void testNoFixpointAllBigger() throws Exception {
        int[] array = new int[]{1, 5, 17, 32, 45};
        Fixpoint.getFixpoint(array);
    }

    @Test
    public void testBiggerArrayArray() throws Exception {
        int[] array = new int[]{-50, -12, -5, 0, 4, 7, 15, 21};

        int index = Fixpoint.getFixpoint(array);

        assertEquals(4, index);
    }
}
