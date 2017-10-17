package ch.isageek.ads.p5.list;

import ch.isageek.ads.p5.NodeAlreadyDefinedException;

import java.util.*;

public class GraphList {

    final private Map<String, Node> nodes;

    public GraphList() {
        this(0);
    }

    public GraphList(int nodes) {
        this.nodes = new HashMap<>(nodes);
    }

    public Node addNode(final String label) throws NodeAlreadyDefinedException {
        if (nodes.containsKey(label)) {
            throw new NodeAlreadyDefinedException(String.format("Node %s is already defined.", label));
        }
        return nodes.put(label, new Node(label));
    }

    public Node getNode(final String label) throws NoSuchElementException {
        Node node =  nodes.get(label);
        if (node == null) {
            throw new NoSuchElementException(String.format("Node %s does not exist.", label));
        }
        return node;
    }

    public void addEdge(final String src, final String dest, final int cost) {
        Node source = this.getNode(src);
        Node destination = this.getNode(dest);
        source.addEdge(new Edge(destination, cost));
    }

    public void removeEdge(final String src, final String dest) {
        Node source = this.getNode(src);
        Node destination = this.getNode(dest);
        source.removeEdge(source.getEdgeTo(destination));
    }


}
