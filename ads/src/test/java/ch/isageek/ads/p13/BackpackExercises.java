package ch.isageek.ads.p13;

import org.junit.Test;

import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class BackpackExercises {


    private static final int[] EXERCISE_WEIGHTS = new int[]{153, 54, 191, 66, 239, 137, 148, 249};
    private static final int[] EXERCISE_VALUES = new int[]{232, 73, 201, 50, 141, 79, 48, 38};
    private static final int EXERCISE_MAX_WEIGHT = 645;

    @Test
    public void Exercise1_c() {
        BackpackBruteforce solver = new BackpackBruteforce();

        System.out.println(String.format("Packing Things with weights %s \nand values %s \ninto a backpack with a max weight of %d",
                Arrays.toString(EXERCISE_WEIGHTS), Arrays.toString(EXERCISE_VALUES), EXERCISE_MAX_WEIGHT
        ));
        BackpackSolver.Solution solution = solver.solve(EXERCISE_WEIGHTS, EXERCISE_VALUES, EXERCISE_MAX_WEIGHT);
        System.out.println(String.format("The fully packed Backpack has a total Value of %d \nand packed the things: %s \nfor a total weight of %d.",
                solution.getValue(), solution.getThings().stream().map(Object::toString).collect(Collectors.joining(", ")), solution.getWeight()
        ));
    }

    @Test
    public void Exercise1_d() {
        BackpackBruteforce solver = new BackpackBruteforce();

        IntStream.range(1, 1000).forEach(size -> {
            double start = System.currentTimeMillis();
            solver.solve(IntStream.range(0, size).toArray(), IntStream.range(0, size).toArray(), size * 2);
            double stop = System.currentTimeMillis();
            double timeInSeconds = (stop - start) / 1000;
            System.out.println(String.format("%d\t%.3f", size, timeInSeconds));
            if (timeInSeconds > 120) {
                throw new RuntimeException(String.format("computation with %d elements already takes %s seconds. Longer is not feasible.", size, timeInSeconds));
            }
        });
    }
}
