package ch.isageek.ads.p5.list;

import ch.isageek.ads.p5.Edge;

public class EdgeL implements Edge {
    final private NodeL destination;
    final private int cost;

    public EdgeL(NodeL destination, int cost) {
        this.destination = destination;
        this.cost = cost;
    }

    @Override
    public NodeL getDestination() {
        return destination;
    }

    @Override
    public int getCost() {
        return cost;
    }
}
