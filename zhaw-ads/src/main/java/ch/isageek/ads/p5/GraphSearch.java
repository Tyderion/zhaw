package ch.isageek.ads.p5;

import java.util.*;

public class GraphSearch {

    private static enum Color {
        WHITE, GRAY, BLACK;
    }

    private static class BFSItem {
        Color color;
        int cost;
        Node pred;
        public BFSItem(Color color, int cost, Node pred) {
            this.color = color;
            this.cost = cost;
            this.pred = pred;
        }
    }

    public static List<SearchResult> breadthFirstSearch(Graph graph, String nodeStart) throws NoSuchElementException {
        Queue<Node> queue = new LinkedList<>();
        List<SearchResult> results = new ArrayList<>(graph.getNumberOfNodes());

        HashMap<Node, BFSItem> table = new HashMap<>(graph.getNumberOfNodes());
        graph.getNodes().forEach(node -> table.put(node, new BFSItem(Color.WHITE, Integer.MAX_VALUE, null)));
        table.get(graph.getNode(nodeStart)).color = Color.GRAY;
        table.get(graph.getNode(nodeStart)).cost = 0;

        queue.add(graph.getNode(nodeStart));
        while (!queue.isEmpty()) {
            Node node = queue.poll();
            List<Edge> edges = node.getEdges();
            for (Edge edge : edges) {
                if (table.get(edge.getDestination()).color == Color.WHITE) {
                    BFSItem item = table.get(edge.getDestination());
                    item.color = Color.GRAY;
                    item.cost = table.get(node).cost + edge.getCost();
                    item.pred = node;
                    queue.add(edge.getDestination());
                }
            }
            table.get(node).color = Color.BLACK;
            results.add(new SearchResult(node, table.get(node).cost));
        }
        return results;
    }

    public static class SearchResult {
        private final Node node;
        private final int cost;

        public SearchResult(Node node, int cost) {
            this.node = node;
            this.cost = cost;
        }

        public Node getNode() {
            return node;
        }

        public int getCost() {
            return cost;
        }

    }
}
