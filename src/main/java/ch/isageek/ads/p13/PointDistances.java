package ch.isageek.ads.p13;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.Arrays.asList;

public class PointDistances {

    private final List<Integer> distances;

    private PointDistances() {
        distances = Collections.emptyList();
    }

    private PointDistances(int[] distances) {
        this.distances = Collections.unmodifiableList(Arrays.stream(distances).boxed().collect(Collectors.toList()));
    }

    public static int[] findPoints(int[] distances) {
        final PointDistances solver = new PointDistances(distances);
        return solver.solve();
    }

    private int[] solve() {
        return new int[0];
    }

    private Result validate(List<Integer> selectedPoints) {
        List<Integer> distances = computeDistances(selectedPoints);
        List<Integer> selectedDistances = new ArrayList<>();
        for (Integer distance : distances) {
            if (this.distances.contains(distance)) {
                selectedDistances.add(distance);
            } else {
                return Result.INVALID;
            }
        }
        if (selectedDistances.size() == this.distances.size()) {
            return Result.SOLUTION;
        }
        return Result.VALID;
    }

    private List<Integer> computeDistances(List<Integer> selectedPoints) {
        List<Integer> distances = new ArrayList<>();
        for (int i = 0; i < selectedPoints.size(); i++) {
            int p1 = selectedPoints.get(i);
            for (int j = i + 1; j < selectedPoints.size(); j++) {
                int p2 = selectedPoints.get(j);
                distances.add(Math.abs(p1 - p2));
            }
        }
        return distances;
    }

    public enum Result {
        VALID, INVALID, SOLUTION
    }


}
