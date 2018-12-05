package serie1;

import org.junit.Assert;
import org.junit.Test;

import static org.unitils.reflectionassert.ReflectionAssert.assertReflectionEquals;

public class SieveOfErathostenesTest {

    @Test
    public void testSimplePrimes() {
        int[] expected = {2, 3, 5, 7, 11, 13, 17, 19, 23};

        int[] result = SieveOfErathostenes.generatePrimes(24);

        assertReflectionEquals(expected, result);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMaxValueOne() {
        SieveOfErathostenes.generatePrimes(1);
    }

    @Test
    public void testMorePrimes() {
        final int maxValue = 1000000;
        final int[] expected = SimplePrimes.generatePrimes(maxValue);

        final int[] result = SieveOfErathostenes.generatePrimes(maxValue);

        assertReflectionEquals(expected, result);
    }
}
