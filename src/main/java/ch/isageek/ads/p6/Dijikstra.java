package ch.isageek.ads.p6;

import ch.isageek.ads.p5.Graph;
import ch.isageek.ads.p5.Node;
import ch.isageek.ads.p5.exception.NodeAlreadyDefinedException;

import java.util.HashMap;
import java.util.NoSuchElementException;
import java.util.PriorityQueue;
import java.util.Queue;

public class Dijikstra<T extends Graph> {

    private final Graph graph;
    private final HashMap<String, DistanceObject> distances;
    private final Class<T> graphClass;

    public Dijikstra(Graph graph, Class<T> graphClass) {
        this.graph = graph;
        this.distances = new HashMap<>(graph.getNumberOfNodes());
        this.graphClass = graphClass;
    }

    /**
     * Computes the shortest path between the nodes with value start and end
     *
     * @param start The value of the start node
     * @param end The value of the end node
     * @return A graph which contains just the path from start to end
     * @throws NoSuchElementException if either start or end node does not exist
     * @throws InstantiationException if the graphclass cannot be instantiated
     * @throws IllegalAccessException if the graphclass cannot be instantiated
     */
    public Path computePath(String start, String end) throws NoSuchElementException, InstantiationException, IllegalAccessException, NodeAlreadyDefinedException {
        Node startNode = graph.getNode(start);
        Node endNOde = graph.getNode(end);
        graph.getNodes().forEach(node -> distances.put(node.getValue(), new DistanceObject(Integer.MAX_VALUE, 0, null)));
        distances.get(startNode.getValue()).setDistance(0);

        Queue<Node> queue = new PriorityQueue<>(graph.getNumberOfNodes(), (left, right) -> {
            DistanceObject leftDistance = distances.get(left.getValue());
            DistanceObject rightDistance = distances.get(right.getValue());
            return Integer.compare(leftDistance.getDistance(), rightDistance.getDistance());
        });
        queue.addAll(graph.getNodes());
        while(!queue.isEmpty()) {
            final Node current = queue.poll();
            current.getEdges().forEach(edge -> {
                Node next = edge.getDestination();
                DistanceObject currDistance = distances.get(current.getValue());
                DistanceObject nextDistance = distances.get(next.getValue());
                if (currDistance.getDistance() + edge.getCost() < nextDistance.getDistance()) {
                    nextDistance.setDistance(currDistance.getDistance() + edge.getCost());
                    nextDistance.setDistanceToPredecessor(edge.getCost());
                    nextDistance.setPredecessor(current.getValue());
                }
            });
        }

        return createResultPath(start, end);
    }

    private Path createResultPath(String start, String end) throws IllegalAccessException, InstantiationException, NodeAlreadyDefinedException {
        Graph graph = graphClass.newInstance();
        DistanceObject current = this.distances.get(end);
        String currentNode = end;
        graph.addNode(currentNode);
        while (current.getPredecessor() != null) {
            graph.addNode(current.getPredecessor());
            graph.addEdge(current.getPredecessor(),currentNode, current.getDistanceToPredecessor());
            currentNode = current.getPredecessor();
            current = distances.get(current.getPredecessor());
        }

        return new Path(graph, distances.get(end).getDistance());
    }

    private static class DistanceObject {
        private int distance;
        private int distanceToPredecessor;
        private String predecessor;

        public DistanceObject(int distance, int distanceToPredecessor, String predecessor) {
            this.distance = distance;
            this.distanceToPredecessor = distanceToPredecessor;
            this.predecessor = predecessor;
        }

        public int getDistance() {
            return distance;
        }

        public void setDistance(int distance) {
            this.distance = distance;
        }

        public int getDistanceToPredecessor() {
            return distanceToPredecessor;
        }

        public void setDistanceToPredecessor(int distanceToPredecessor) {
            this.distanceToPredecessor = distanceToPredecessor;
        }

        public String getPredecessor() {
            return predecessor;
        }

        public void setPredecessor(String predecessor) {
            this.predecessor = predecessor;
        }
    }

    public static class Path {
        private Graph path;
        private int length;

        public Path(Graph path, int length) {
            this.path = path;
            this.length = length;
        }

        public Graph getGraph() {
            return path;
        }

        public int getLength() {
            return length;
        }
    }
}
