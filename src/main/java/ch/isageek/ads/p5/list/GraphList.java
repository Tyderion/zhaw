package ch.isageek.ads.p5;

import java.util.*;

public class GraphList {
    static class Edge {
        final Node destination;
        final int cost;

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

    static class Node {
        final String label;

        List<Edge> adjList = new ArrayList<>();

        public Node(String label) {
            this.label = label;
        }

        public void addEdge(Edge e) {
            adjList.add(e);
        }

        public Iterator<Edge> getEdges() {
            return adjList.iterator();
        }

        public Edge getEdgeTo(Node n) {
            return adjList.stream().filter(edge -> edge.destination.equals(n)).findFirst().orElse(null);
        }
    }

    final private Map<String, Node> nodes;

    public GraphList(int nodes) {
        this.nodes = new HashMap<>(nodes);
    }

    

}
