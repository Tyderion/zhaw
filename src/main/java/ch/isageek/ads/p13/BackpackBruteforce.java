package ch.isageek.ads.p13;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class BackpackBruteforce implements BackpackSolver {

    @Override
    public Solution solve(final List<Thing> things, final int maxWeight) throws IllegalArgumentException {
        if (maxWeight <= 0) {
            throw new IllegalArgumentException("MaxWeight should be positive.");
        }
        final List<Thing> allThings = Collections.unmodifiableList(things);
        final int maxItems = allThings.size();
        // For each possible length of a subselection of the things
        // Try every combination
        final Solution[] bestSolution = {null};
        for (int size = 1; size < maxItems; size++) {
           List<int[]> combos = getKCombinations(size, maxItems);
           combos.forEach(combo -> {
               Solution current = new Solution(maxWeight);
               Arrays.stream(combo).forEach(index -> current.addThing(things.get(index)));
               if (current.isBetter(bestSolution[0])) {
                   bestSolution[0] = current;
               }
           });
        }
        return bestSolution[0];
    }

    /**
     * Returns a list of all combinations of the form [0, 1, 2}, [0, 1, 3], [0, 2, 3], [1, 2, 3]
     *
     * @param k     The size of the combinations
     * @param maxIndex The max value for each position
     * @return A list of possible cominations
     */
    private List<int[]> getKCombinations(int k, int maxIndex) {
        List<int[]> subsets = new ArrayList<>();
        int[] combination = new int[k];                  // here we'll keep indices

        if (k <= maxIndex) {
            // first index sequence: 0, 1, 2, ...
            for (int i = 0; (combination[i] = i) < k - 1; i++);
            subsets.add(combination.clone());
            for(;;) {
                int i;
                // find position of item that can be incremented
                for (i = k - 1; i >= 0 && combination[i] == maxIndex - k + i; i--);
                if (i < 0) {
                    break;
                }
                combination[i]++;                    // increment this item
                for (++i; i < k; i++) {    // fill up remaining items
                    combination[i] = combination[i - 1] + 1;
                }
                subsets.add(combination.clone());
            }
        }
        return subsets;
    }
    // generate actual subset by index sequence
    int[] getSubset(int[] input, int[] subset) {
        int[] result = new int[subset.length];
        for (int i = 0; i < subset.length; i++)
            result[i] = input[subset[i]];
        return result;
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
