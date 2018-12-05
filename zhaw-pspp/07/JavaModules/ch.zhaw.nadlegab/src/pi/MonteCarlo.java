package pi;

import random.HighQualityRandom;

import java.util.Random;

public class MonteCarlo {
    public static double computePi(int steps) {
        double x, y;

        int count = 0;

        Random random = new HighQualityRandom();
        for (int i = 0; i < steps; i++) {
            x = random.nextDouble();
            y = random.nextDouble();
            if (Math.pow(x, 2) + Math.pow(y, 2) <= 1) {
                count++;
            }
        }

        return count / (double)steps * 4;
    }
}
