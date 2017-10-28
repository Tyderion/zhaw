package ch.isageek.ads.p6;

import ch.isageek.ads.p5.Graph;
import ch.isageek.ads.p5.Node;

import java.util.NoSuchElementException;
import java.util.PriorityQueue;

public class Dijikstra {

    private final Graph graph;

    public Dijikstra(Graph graph) {
        this.graph = graph;
    }

    /**
     * Computes the shortest path between the nodes with value start and end
     *
     * @param start The value of the start node
     * @param end The value of the end node
     * @return A graph which contains just the path from start to end
     * @throws NoSuchElementException if either start or end node does not exist
     */
    public Path computePath(String start, String end) throws NoSuchElementException {
        PriorityQueue<Node> unsettledNodes;
        return null;
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
