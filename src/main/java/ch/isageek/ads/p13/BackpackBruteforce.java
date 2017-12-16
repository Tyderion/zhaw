package ch.isageek.ads.p13;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public class BackpackBruteforce implements BackpackSolver {

    @Override
    public Solution solve(final List<Thing> things, final int maxWeight) throws IllegalArgumentException {
        if (maxWeight <= 0) {
            throw new IllegalArgumentException("MaxWeight should be positive.");
        }
        final List<Thing> allThings = Collections.unmodifiableList(things);
        final int maxItems = allThings.size();
//
//        for (int i = 1; i < 5; i++) {
//            List<int[]> subsets = getKCombinations(i, 5);
//            System.out.println(subsets.stream().map(Arrays::toString).collect(Collectors.joining(", ")));
//        }

//        return null;

        // For each possible length of a subselection of the things
        // Try every combination
        final Solution[] bestSolution = {null};
        for (int size = 1; size < maxItems; size++) {
            forEachKCombinations(size, maxItems, indeces -> {
                Solution current = Solution.fromIndeces(indeces, things, maxWeight);
                if (current.isBetter(bestSolution[0])) {
                    bestSolution[0] = current;
                }
            });
        }
        return bestSolution[0];
    }

    /**
     * Exectues an action for each k-combination of the form [0, 1, 2}, [0, 1, 3], [0, 2, 3], [1, 2, 3]
     * This method allocates only one int array of size k, so the space requirements are k.
     *
     * @param k        The size of the combinations
     * @param maxIndex The max value for each position
     */
    private void forEachKCombinations(int k, int maxIndex, Consumer<int[]> action) {
        int[] combination = new int[k];                  // here we'll keep indices
        if (k <= maxIndex) {
            // first index sequence: 0, 1, 2, ...
            for (int i = 0; i < k; i++) {
                combination[i] = i;
            }
            action.accept(combination.clone());
            while (true) {
                int pos;
                // find position of item that can be incremented
                for (pos = k - 1; pos >= 0; pos--) {
                    if (combination[pos] < maxIndex - k + pos) {
                        break;
                    }
                }
                if (pos < 0) {
                    break;
                }
                combination[pos]++;                    // increment this item
                for (int rest = pos +1; rest < k; rest++) {
                    combination[rest] = combination[rest - 1] + 1;
                }
                action.accept(combination.clone());
            }
        }
    }

    @Override
    public Solution solve(final int[] weights, final int[] values, final int maxValue) throws IllegalArgumentException {
        if (weights.length != values.length) {
            throw new IllegalArgumentException("We need a weight for each value");
        }
        List<Thing> things = new ArrayList<>();
        for (int i = 0; i < weights.length; i++) {
            things.add(new Thing(weights[i], values[i]));
        }
        return solve(things, maxValue);
    }
}
