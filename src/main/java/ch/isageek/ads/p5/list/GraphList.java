package ch.isageek.ads.p5.list;

import ch.isageek.ads.p5.*;

import java.io.File;
import java.util.*;

public class GraphList extends BaseGraph {

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
}
