package ch.isageek.ads.p5.list;

import ch.isageek.ads.p5.Edge;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class NodeL implements ch.isageek.ads.p5.Node {
    final private String label;

    private List<EdgeL> adjList = new ArrayList<>();

    public NodeL(String label) {
        this.label = label;
    }

    public void addEdge(EdgeL e) {
        adjList.add(e);
    }

    public List<Edge> getEdges() {
        return new ArrayList<>(adjList);
    }

    public EdgeL getEdgeTo(NodeL n) {
        return adjList.stream().filter(edge -> edge.getDestination().equals(n)).findFirst().orElse(null);
    }

    public void removeEdgeTo(NodeL n) {
        EdgeL e = getEdgeTo(n);
        if (e != null) {
            adjList = adjList.stream().filter(edge -> !edge.equals(e)).collect(Collectors.toList());
        }
    }

    public int getEdgeCount() {
        return adjList.size();
    }

    @Override
    public String getValue() {
        return label;
    }




}
