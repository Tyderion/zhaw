package ch.isageek.ads.p5.list;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

public class Node {
    final private String label;

    private List<Edge> adjList = new ArrayList<>();

    public Node(String label) {
        this.label = label;
    }

    public void addEdge(Edge e) {
        adjList.add(e);
    }

    public List<Edge> getEdges() {
        return new ArrayList<>(adjList);
    }

    public Edge getEdgeTo(Node n) {
        return adjList.stream().filter(edge -> edge.getDestination().equals(n)).findFirst().orElse(null);
    }

    public void removeEdgeTo(Node n) {
        Edge e = getEdgeTo(n);
        if (e != null) {
            adjList = adjList.stream().filter(edge -> !edge.equals(e)).collect(Collectors.toList());
        }
    }
}
