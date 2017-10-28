package ch.isageek.ads.p6;

import ch.isageek.ads.p5.Edge;
import ch.isageek.ads.p5.Graph;
import ch.isageek.ads.p5.Node;
import ch.isageek.ads.p5.exception.GraphParseException;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.List;
import java.util.stream.Collectors;

public class Exercise4 {
    private static final String FILENAME = "swiss_cities.csv";
    final UndirectedGraphList graph;
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

    public void c() {
        Dijikstra dijikstra = new Dijikstra(graph);
        Dijikstra.Path result = dijikstra.computePath("Winterthur", "Lugano");

        System.out.println(String.format("Kürzester Weg von Winterthur nach Lugano ist %dkm lang.", result.getLength()));
        Node node = result.getStart();
        while (node.getEdges().size() > 0) {
            Edge toNext = node.getEdges().get(0);
            System.out.println(String.format("%s -> %s: %d", node.getValue(), toNext.getDestination().getValue(), toNext.getCost()));
            node = toNext.getDestination();
        }
    }

    public int computeTotalDistance() {
        List<Edge> edges = graph.getNodes().stream().flatMap(node -> node.getEdges().stream()).collect(Collectors.toList());
        return edges.stream().mapToInt(Edge::getCost).sum() / 2; // we have a directed graph underneath so we count every edge double
    }
}
