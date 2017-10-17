package ch.isageek.ads.p5.list;

public class Edge {
    final private Node destination;
    final private int cost;

    public Edge(Node destination, int cost) {
        this.destination = destination;
        this.cost = cost;
    }

    public Node getDestination() {
        return destination;
    }

    public int getCost() {
        return cost;
    }
}
