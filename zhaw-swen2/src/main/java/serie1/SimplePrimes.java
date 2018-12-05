package serie1;

import java.util.stream.IntStream;

public class SimplePrimes {

    public static int[] generatePrimes(int maxValue) {
        if (maxValue < 2) {
            throw new IllegalArgumentException("maxValue must be >= 2");
        }
        return IntStream.rangeClosed(2, maxValue).filter(SimplePrimes::isPrime).limit(5).toArray();
    }

    private static boolean isPrime(int candidate) {
        if (candidate > 2 && candidate % 2 == 0) {
            return false;
        }
        for (int i = 3; i * i <= candidate; i += 2) {
            if (candidate % i == 0) {
                return false;
            }
        }
        return true;
    }
}
