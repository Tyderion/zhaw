package ch.isageek.ads.p5.impl;

import ch.isageek.ads.p5.*;
import ch.isageek.ads.p5.exception.NodeAlreadyDefinedException;

import java.util.*;
import java.util.stream.Collectors;

/**
 * This class uses nested Hashmaps to simulate a matrix
 * This is more convenient than actualy matrices (or lists in lists) when removing nodes.
 * When we have an actual matrix (e.g. 2d array) we need to move a lot of elements around when removing nodes
 */
public class GraphHashmapMatrix extends LoadingGraph {

    private Map<String, Map<String, Integer>> adjacencyMatrix;

    public GraphHashmapMatrix() {
        adjacencyMatrix = new HashMap<>(0);
    }

    @Override
    public int getNumberOfNodes() {
        return adjacencyMatrix.size();
    }

    @Override
    public int getNumberOfEdges() {
        return adjacencyMatrix.values().stream()
                .mapToInt(row -> (int)row.values().stream().filter(value -> value != 0).count()).sum();
    }

    @Override
    public List<Node> getNodes() {
        return adjacencyMatrix.keySet().stream().map(this::toNode).collect(Collectors.toList());
    }

    @Override
    public List<Edge> getEdgesFor(String label) {
        if (!adjacencyMatrix.containsKey(label)) {
            throw new NoSuchElementException(String.format("Node %s does not exist.", label));
        }
        return adjacencyMatrix.get(label).entrySet().stream()
                .filter(entry -> entry.getValue() != 0)
                .map(entry -> new Edge() {
                    @Override
                    public Node getDestination() {
                        return toNode(entry.getKey());
                    }

                    @Override
                    public int getCost() {
                        return entry.getValue();
                    }
                }).collect(Collectors.toList());
    }

    @Override
    public Node addNode(String label) throws NodeAlreadyDefinedException {
        if (adjacencyMatrix.containsKey(label)) {
            throw new NodeAlreadyDefinedException(label);
        }
        Map<String, Integer> values = new HashMap<>();
        adjacencyMatrix.keySet().forEach(key -> {
            adjacencyMatrix.get(key).put(label, 0);
            values.put(key, 0);
        });
        adjacencyMatrix.put(label, values);
        return toNode(label);
    }

    @Override
    public Node getNode(final String label) throws NoSuchElementException {
        if (adjacencyMatrix.containsKey(label)) {
            return toNode(label);
        } else {
            throw new NoSuchElementException(String.format("Node %s does not exist", label));
        }
    }

    @Override
    public void removeNode(String label) throws NoSuchElementException {
        if (adjacencyMatrix.containsKey(label)) {
            adjacencyMatrix.remove(label);
            adjacencyMatrix.keySet().forEach(key -> {
                adjacencyMatrix.get(key).remove(label);
            });
        } else {
            throw new NoSuchElementException(String.format("Node %s does not exist", label));
        }
    }

    @Override
    public void addEdge(String src, String dest, int cost) {
        if (!adjacencyMatrix.containsKey(src) || !adjacencyMatrix.containsKey(dest)) {
            throw new NoSuchElementException();
        }
        adjacencyMatrix.get(src).put(dest, cost);
    }

    @Override
    public void removeEdge(String src, String dest) {
        if (!adjacencyMatrix.containsKey(src) || !adjacencyMatrix.containsKey(dest)) {
            throw new NoSuchElementException();
        }
        adjacencyMatrix.get(src).put(dest, 0);
    }


    private Node toNode(String label) {
        return toNode(label, new HashMap<>(adjacencyMatrix.size()));
    }
    private Node toNode(String label, Map<String, Node> nodes) {
        if (!adjacencyMatrix.containsKey(label)) {
            throw new NoSuchElementException(String.format("Node %s does not exist", label));
        }
        Map<String, Integer> edges = adjacencyMatrix.get(label);
        if (!nodes.containsKey(label)) {
            nodes.put(label, new Node() {
                @Override
                public String getValue() {
                    return label;
                }

                @Override
                public List<Edge> getEdges() {
                    return edges.entrySet().stream().filter(entry -> entry.getValue() != 0).map(entry -> toEdge(entry.getKey(), entry.getValue(), nodes)).collect(Collectors.toList());
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

    private Edge toEdge(String dest, int cost, Map<String, Node> nodes) {
        return new Edge() {
            @Override
            public Node getDestination() {
                return toNode(dest, nodes);
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
