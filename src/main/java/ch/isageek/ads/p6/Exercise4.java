package ch.isageek.ads.p6;

import ch.isageek.ads.p5.Edge;
import ch.isageek.ads.p5.Node;
import ch.isageek.ads.p5.exception.GraphParseException;
import ch.isageek.ads.p5.impl.GraphList;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public class Exercise4 {
    private static final String FILENAME = "swiss_cities.csv";
    private final UndirectedGraphList graph;
    public static void main(String[] args) throws FileNotFoundException, URISyntaxException {
        // Load file into graph
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        URL path = classloader.getResource(FILENAME);
        if (path == null) {
            throw new FileNotFoundException(FILENAME);
        }
        Exercise4 exercise = new Exercise4(new File(path.toURI()));
        exercise.a();
    }

    public Exercise4(File file) throws FileNotFoundException {
        graph = new UndirectedGraphList();
        try {
            graph.readFromFile(file);
        } catch (IOException | GraphParseException e) {
            throw new RuntimeException(e);
        }
    }

    public void a() {
        System.out.println("Exercise 4a)");
        System.out.println(String.format("Total Distance: %dkm", computeTotalDistance()));
    }

    public void c() throws Exception {
        final String start = "Winterthur";
        final String end = "Lugano";
        // The path is directional from start to end so we use a directed graph for representation
        Dijkstra dijkstra = new Dijkstra<>(graph, GraphList.class);
        Dijkstra.Path result = dijkstra.computePath(start, end);
        System.out.println("Exercise 4b)");

        System.out.println(String.format("Shortest path from %s to %s is %dkm long.",start, end, result.getLength()));
        Node node = result.getGraph().getNode(start);
        while (node.getEdges().size() > 0) {
            Edge toNext = node.getEdges().get(0);
            System.out.println(String.format("%s -> %s: %dkm", node.getValue(), toNext.getDestination().getValue(), toNext.getCost()));
            node = toNext.getDestination();
        }
    }

    public int computeTotalDistance() {
        List<Edge> edges = graph.getNodes().stream().flatMap(node -> node.getEdges().stream()).collect(Collectors.toList());
        return edges.stream().mapToInt(Edge::getCost).sum() / 2; // we have a directed graph underneath so we count every edge double
    }
}
