package serie1;

import java.util.Arrays;
import java.util.Scanner;

public class Serie1 {
    public static void main(String[] args) {
        System.out.println("Please enter a maximum number to generate primes up to");
        int[] primes = null;
        int maxValue = 0;
        while(primes == null) {
            try {
                Scanner scanner = new Scanner(System.in);
                maxValue = scanner.nextInt();

                primes = SieveOfErathostenes.generatePrimes(maxValue);
            } catch (IllegalArgumentException e) {
                System.out.println("Please enter a max value >= 2");
            }
        }

        System.out.println(String.format("Primes up to %d: %s", maxValue,Arrays.toString(primes)));
    }
}
