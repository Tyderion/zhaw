package ch.isageek.ads.p5.impl;

import ch.isageek.ads.p5.Edge;
import ch.isageek.ads.p5.Node;
import ch.isageek.ads.p5.exception.NodeAlreadyDefinedException;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * This class uses 2 nested arraylists to simulate a matrix.
 * Additionally node labels are mapped to an index to access the matrix
 **/
public class GraphMatrix extends LoadingGraph {

    private List<List<Integer>> adjacencyMatrix = new ArrayList<>();
    private Map<Integer, String> indexToNode = new HashMap<>();
    private Map<String, Integer> nodeToIndex = new HashMap<>();

    public GraphMatrix() {
    }

    @Override
    public int getNumberOfNodes() {
        return adjacencyMatrix.size();
    }

    @Override
    public int getNumberOfEdges() {
        return adjacencyMatrix.stream().mapToInt(sublist -> (int)sublist.stream().filter(value -> value != 0).count()).sum();
    }

    @Override
    public List<Node> getNodes() {
        return IntStream.range(0, adjacencyMatrix.size()).mapToObj(this::toNode).collect(Collectors.toList());
    }

    @Override
    public List<Edge> getEdgesFor(String label) {
        if (!nodeToIndex.containsKey(label)) {
            throw new NoSuchElementException(String.format("Node %s does not exist.", label));
        }
        final int index = nodeToIndex.get(label);
        final List<Integer> edgeWeights = adjacencyMatrix.get(index);
        return IntStream.range(0, edgeWeights.size())
                .filter(idx -> edgeWeights.get(idx) != 0)
                .mapToObj(idx -> new Edge() {
                    @Override
                    public Node getDestination() {
                        return toNode(idx);
                    }

                    @Override
                    public int getCost() {
                        return edgeWeights.get(idx);
                    }
                }).collect(Collectors.toList());
    }

    @Override
    public Node addNode(String label) throws NodeAlreadyDefinedException {
        if (nodeToIndex.containsKey(label)) {
            throw new NodeAlreadyDefinedException(label);
        }
        final int index = adjacencyMatrix.size();
        adjacencyMatrix.add(index, new ArrayList<>());

        nodeToIndex.put(label, index);
        indexToNode.put(index, label);

        List<Integer> weights = adjacencyMatrix.get(index);
        IntStream.range(0, adjacencyMatrix.size()).forEach(idx -> {
            weights.add(idx, 0);
            adjacencyMatrix.get(idx).add(0);
        });
        return toNode(index);
    }

    @Override
    public Node getNode(final String label) throws NoSuchElementException {
        if (nodeToIndex.containsKey(label)) {
            return toNode(nodeToIndex.get(label));
        } else {
            throw new NoSuchElementException(String.format("Node %s does not exist", label));
        }
    }

    @Override
    public void removeNode(final String label) throws NoSuchElementException {
        if (nodeToIndex.containsKey(label)) {
            final int index = nodeToIndex.get(label);
            // Remove weights and node
            adjacencyMatrix.forEach(weights -> weights.remove(index));
            adjacencyMatrix.remove(index);

            // Update indeces
            IntStream.range(index+1, adjacencyMatrix.size())
                    .forEach(idx -> {
                        String idxNode = indexToNode.get(idx);
                        indexToNode.put(idx-1, idxNode);
                        nodeToIndex.put(idxNode, idx-1);
                    });
        } else {
            throw new NoSuchElementException(String.format("Node %s does not exist", label));
        }
    }

    @Override
    public void addEdge(String src, String dest, int cost) {
        if (!nodeToIndex.containsKey(src) || !nodeToIndex.containsKey(dest)) {
            throw new NoSuchElementException();
        }
        adjacencyMatrix.get(nodeToIndex.get(src)).set(nodeToIndex.get(dest), cost);
    }

    @Override
    public void removeEdge(String src, String dest) {
        if (!nodeToIndex.containsKey(src) || !nodeToIndex.containsKey(dest)) {
            throw new NoSuchElementException();
        }
        adjacencyMatrix.get(nodeToIndex.get(src)).set(nodeToIndex.get(dest), 0);
    }


    private Node toNode(int index) {
        return toNode(index, new HashMap<>(adjacencyMatrix.size()));
    }
    private Node toNode(int index, Map<String, Node> nodes) {

        if (adjacencyMatrix.size() <= index) {
            throw new NoSuchElementException(String.format("Node %d does not exist", index));
        }
        String label = indexToNode.get(index);
        final List<Integer> edges = adjacencyMatrix.get(index);
        if (!nodes.containsKey(label)) {
            nodes.put(label, new Node() {
                @Override
                public String getValue() {
                    return label;
                }

                @Override
                public List<Edge> getEdges() {
                    return IntStream.range(0, edges.size()).mapToObj(idx -> toEdge(idx, edges.get(idx), nodes)).filter(edge -> edge.getCost() != 0).collect(Collectors.toList());
                }

                @Override
                public int hashCode() {
                    return Objects.hash(label, getEdges().size());
                }

                @Override
                public boolean equals(Object obj) {
                    return Objects.equals(this.hashCode(), obj.hashCode());
                }
            });
        }

        return nodes.get(label);
    }

    private Edge toEdge(int destIndex, int cost, Map<String, Node> nodes) {
        return new Edge() {
            @Override
            public Node getDestination() {
                return toNode(destIndex, nodes);
            }

            @Override
            public int getCost() {
                return cost;
            }

            @Override
            public int hashCode() {
                return Objects.hash(getDestination(), cost);
            }

            @Override
            public boolean equals(Object obj) {
                return Objects.equals(this.hashCode(), obj.hashCode());
            }
        };
    }
}
