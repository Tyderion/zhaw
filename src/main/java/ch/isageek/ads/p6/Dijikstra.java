package ch.isageek.ads.p6;

import ch.isageek.ads.p5.Graph;
import ch.isageek.ads.p5.Node;

import java.util.NoSuchElementException;

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
     * @return A path which contains toatl length and th estart node with just one edge leading to other nodes with one edge until the final node which is the end
     * @throws NoSuchElementException if either start or end node does not exist
     */
    public Path computePath(String start, String end) throws NoSuchElementException {
        return null;
    }

    public static class Path {
        private Node start;
        private int length;

        public Path(Node start, int length) {
            this.start = start;
            this.length = length;
        }

        public Node getStart() {
            return start;
        }

        public int getLength() {
            return length;
        }
    }
}
