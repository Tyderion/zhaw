package ch.isageek.ads.p13;

import java.util.*;
import java.util.stream.Collectors;

import static java.util.Arrays.asList;

public class PointDistances {

    private final List<Integer> distances;
    private final int distancesCount;
    private List<Integer> chosenDistances;

    private PointDistances() {
        distances = Collections.emptyList();
        chosenDistances = new ArrayList<>();
        distancesCount = 0;
    }

    private PointDistances(int[] distances) {
        this.distances = Collections.unmodifiableList(Arrays.stream(distances).boxed().collect(Collectors.toList()));
        chosenDistances = new ArrayList<>();
        this.distancesCount = this.distances.size();
    }

    public static int[] findPoints(int[] distances) {
        final PointDistances solver = new PointDistances(distances);
        return solver.solve();
    }

    private int[] solve() {
        List<Integer> currentPoints = new ArrayList<>();
        currentPoints.add(0);
        Try root = new Try(null, currentPoints, Collections.emptyList(), validate(currentPoints));;
        Try currentTry = root;
        while (true) {
            Optional<Integer> nextDistance = getNextDistance(currentTry);
            if (nextDistance.isPresent()) {
                Integer nextDi = nextDistance.get();
                List<Integer> selectedDistances = currentTry.withAddedDistance(nextDi);
                List<Integer> selectedPoints = currentTry.pointsWithAddedDistance(nextDi);
                Try nextTry = new Try(currentTry,selectedPoints, selectedDistances, validate(selectedPoints));
                currentTry.addTry(nextDi, nextTry);
                currentTry = nextTry;
                switch (currentTry.getResult()) {
                    case SOLUTION:
                        return currentTry.getSelectedPoints().stream().mapToInt(i->i).toArray();
                    case VALID:
                        continue;
                    case INVALID:
                        Try lastTry = currentTry.getLastTry();
                        while (!lastTry.hasUntriedOptions(distancesCount)) {
                            lastTry = lastTry.getLastTry();
                            if (lastTry == null) {
                                return new int[]{};
                            }
                        }
                        currentTry = lastTry;
                }
            }
        }
    }


    private Optional<Integer> getNextDistance(final Try aTry) {
        List<Integer> distances = new ArrayList<>(this.distances);
        for (Integer d : aTry.getSelectedDistances()) {
            distances.remove(d);
        }
        for (Integer d : aTry.getNextTriesDistances()) {
            distances.remove(d);
        }
        if (distances.size() > 0) {
            return Optional.of(distances.get(0));
        }
        return Optional.empty();
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
        private final List<Integer> selectedPoints;
        private final List<Integer> selectedDistances;
        private final Result result;
        public Try(Try lastTry, List<Integer> selectedPoints,List<Integer> selectedDistances,  Result result) {
            this.lastTry = lastTry;
            this.selectedPoints = Collections.unmodifiableList(selectedPoints);
            this.selectedDistances = Collections.unmodifiableList(selectedDistances);
            this.result = result;
            this.nextTries = new HashMap<>();
        }

        public List<Integer> getSelectedPoints() {
            return selectedPoints;
        }

        public List<Integer> getSelectedDistances() {
            return selectedDistances;
        }

        public Try getLastTry() {
            return lastTry;
        }

        public Result getResult() {
            return result;
        }

        private boolean hasUntriedOptions(int allDistancesCount) {
            return this.nextTries.size() + this.selectedDistances.size() < allDistancesCount;
        }

        public Set<Integer> getNextTriesDistances() {
            return nextTries.keySet();
        }

        public void addTry(int number, Try aTry) {
            this.nextTries.put(number, aTry);
        }

        public List<Integer> withAddedDistance(int number) {
            List<Integer> distances = new ArrayList<>(selectedDistances);
            distances.add(number);
            return distances;
        }

        public List<Integer> pointsWithAddedDistance(int number) {
            List<Integer> distances = new ArrayList<>(selectedPoints);
            distances.add(distances.get(distances.size() - 1) + number);
            return distances;
        }
    }


}
