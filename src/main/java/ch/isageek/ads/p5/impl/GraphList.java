package ch.isageek.ads.p5.impl;

import ch.isageek.ads.p5.*;
import ch.isageek.ads.p5.exception.NodeAlreadyDefinedException;

import java.util.*;
import java.util.stream.Collectors;

public class GraphList extends LoadingGraph {

    final private Map<String, NodeL> nodes;

    public GraphList() {
        this(0);
    }

    public GraphList(int nodes) {
        this.nodes = new HashMap<>(nodes);
    }

    public NodeL addNode(final String label) throws NodeAlreadyDefinedException {
        if (nodes.containsKey(label)) {
            throw new NodeAlreadyDefinedException(label);
        }
        NodeL n = new NodeL(label);
        nodes.put(label, n);
        return n;
    }

    public void removeNode(final String label) throws NoSuchElementException {
        NodeL node = getNode(label);
        nodes.remove(label);
        nodes.values().forEach(n -> n.removeEdgeTo(node));
    }

    public NodeL getNode(final String label) throws NoSuchElementException {
        NodeL node =  nodes.get(label);
        if (node == null) {
            throw new NoSuchElementException(String.format("NodeL %s does not exist.", label));
        }
        return node;
    }

    public void addEdge(final String src, final String dest, final int cost) {
        NodeL source = this.getNode(src);
        NodeL destination = this.getNode(dest);
        source.addEdge(new EdgeL(destination, cost));
    }

    public void removeEdge(final String src, final String dest) {
        NodeL source = this.getNode(src);
        NodeL destination = this.getNode(dest);
        source.removeEdgeTo(destination);
    }

    @Override
    public int getNumberOfNodes() {
        return nodes.size();
    }

    @Override
    public int getNumberOfEdges() {
        return nodes.values().stream().mapToInt(NodeL::getEdgeCount).sum();
    }

    @Override
    public List<Node> getNodes() {
        return new ArrayList<>(nodes.values());
    }

    @Override
    public List<Edge> getEdgesFor(final String label) {
        return getNode(label).getEdges();
    }

    private static class EdgeL implements Edge {
        final private NodeL destination;
        final private int cost;

        EdgeL(NodeL destination, int cost) {
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

    private static class NodeL implements Node {
        final private String label;

        private List<EdgeL> adjList = new ArrayList<>();

        NodeL(String label) {
            this.label = label;
        }

        void addEdge(EdgeL e) {
            adjList.add(e);
        }

        public List<Edge> getEdges() {
            return new ArrayList<>(adjList);
        }

        EdgeL getEdgeTo(NodeL n) {
            return adjList.stream().filter(edge -> edge.getDestination().equals(n)).findFirst().orElse(null);
        }

        void removeEdgeTo(NodeL n) {
            EdgeL e = getEdgeTo(n);
            if (e != null) {
                adjList = adjList.stream().filter(edge -> !edge.equals(e)).collect(Collectors.toList());
            }
        }

        int getEdgeCount() {
            return adjList.size();
        }

        @Override
        public String getValue() {
            return label;
        }
    }
}
