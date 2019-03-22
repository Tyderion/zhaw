import java.math.BigInteger;

public class Exercise1 {

    private static final int[] ws = new int[] { 128, 256, 384, 512};

    public static void main(String[] args) {
        for (int w : ws) {
            System.out.println(String.format("For w = %d b is %d", w, computeB(w)));
        }
    }

    private static int computeB(int w) {
        double bigW = 0;
        int b = 1;
        while(Math.pow(bigW, 1d/w) < 2) {
            bigW = computeBigW(b);
            b++;
        }
        BigInteger b = BigInteger.valueOf(100);

        b.modPow()

        return b;
    }

    private static double computeBigW(int b) {
        return Math.exp(1.92 * Math.pow(b, 1d/3) * Math.pow(Math.log(b), 2d/3));
    }
}
