package serie1;

import org.junit.Before;
import org.junit.Test;

import static org.unitils.reflectionassert.ReflectionAssert.assertReflectionEquals;

public class SimplePrimesTest {

    @Test
    public void testSimplePrimes() {
        int[] expected = {2, 3, 5, 7, 11, 13, 17, 19, 23};

        int[] result = SimplePrimes.generatePrimes(24);

        assertReflectionEquals(expected, result);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMaxValueOne() {
        SimplePrimes.generatePrimes(1);
    }
}
