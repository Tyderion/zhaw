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

    private class Try {
        private Try lastTry;
        private HashMap<Integer, Try> nextTries;
        private List<Integer> selectedPoints;
        private Result result;
        public Try(Try lastTry, List<Integer> selectedPoints, Result result) {
            this.lastTry = lastTry;
            this.selectedPoints = selectedPoints;
            this.result = result;
            this.nextTries = new HashMap<>();
        }

        public List<Integer> getSelectedPoints() {
            return new ArrayList<>(selectedPoints);
        }

        public Try getLastTry() {
            return lastTry;
        }

        public Result getResult() {
            return result;
        }

        public boolean hasTry(int number) {
            return nextTries.containsKey(number);
        }

        public void addTry(int number, Try aTry) {
            this.nextTries.put(number, aTry);
        }
    }


}
