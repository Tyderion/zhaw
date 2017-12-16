package ch.isageek.ads.p13;

import java.util.ArrayList;
import java.util.List;

public interface BackpackSolver {

    Solution solve(final List<Thing> things, final int maxWeight);

    Solution solve(final int[] weights, final int[] values, final int maxWeight);

    class Solution {
        final List<Thing> things;
        final int maxWeight;

        public Solution(final int maxWeight) {
            this.things = new ArrayList<>();
            this.maxWeight = maxWeight;
        }

        public boolean isValid() {
            return getWeight() <= maxWeight;
        }

        public boolean isBetter(Solution other) {
            if (other == null || !other.isValid()) {
                return isValid();
            }
            return isValid() && getValue() > other.getValue();
        }

        public int size() {
            return things.size();
        }

        public void addThing(Thing thing) {
            things.add(thing);
        }

        public List<Thing> getThings() {
            return things;
        }

        public int getValue() {
            return things.stream().mapToInt(Thing::getValue).sum();
        }
        public int getWeight() {
            return things.stream().mapToInt(Thing::getWeight).sum();
        }

    }

    class Thing {
        private final int weight;
        private final int value;

        public Thing(final int weight,final  int value) {
            this.weight = weight;
            this.value = value;
        }

        public int getWeight() {
            return weight;
        }

        public int getValue() {
            return value;
        }
    }
}
