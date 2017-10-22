package ch.isageek.ads.p5;

import ch.isageek.ads.p5.impl.GraphList;
import org.junit.Test;
import org.unitils.reflectionassert.ReflectionComparatorMode;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.Arrays.asList;
import static junit.framework.TestCase.assertNotNull;

import static ch.isageek.ads.p5.GraphSearch.SearchResult;
import static org.unitils.reflectionassert.ReflectionAssert.assertReflectionEquals;

public class GraphSearchTest {


    @Test
    public void testPeruseAllBFS() throws Exception {
        Graph graph = new GraphList();
        readGraph("graph_peruseall16.2.txt", graph);

        List<String> expectedAsString = asList("1:0", "2:1", "3:1", "4:2", "6:2", "5:3");

        List<SearchResult> results = GraphSearch.breadthFirstSearch(graph, "1");
        List<String> result = results.stream().map(res -> String.format("%s:%d", res.getNode().getValue(), res.getCost())).collect(Collectors.toList());

        // The order of iteration over edges is not deterministic in BFS though all of same depth should be processed after eachother so the resulting data has to be the same
        assertReflectionEquals(expectedAsString, result, ReflectionComparatorMode.LENIENT_ORDER);
    }

    @Test
    public void testSimpleBFS() throws Exception {
        Graph graph = new GraphList();
        readGraph("simple_graph.csv", graph);

        List<String> expectedAsString = asList("Zürich:0", "Bern:1");
        List<SearchResult> results = GraphSearch.breadthFirstSearch(graph, "Zürich");
        List<String> result = results.stream().map(res -> String.format("%s:%d", res.getNode().getValue(), res.getCost())).collect(Collectors.toList());

        // The order of iteration over edges is not deterministic in BFS though all of same depth should be processed after eachother so the resulting data has to be the same
        assertReflectionEquals(expectedAsString, result, ReflectionComparatorMode.LENIENT_ORDER);
    }

    @Test
    public void testSimpleBFSWeighted() throws Exception {
        Graph graph = new GraphList();
        readGraph("simple_graph_weighted.csv", graph);
        List<String> expectedAsString = asList("Zürich:0", "Bern:110");

        List<SearchResult> results = GraphSearch.breadthFirstSearch(graph, "Zürich");
        List<String> result = results.stream().map(res -> String.format("%s:%d", res.getNode().getValue(), res.getCost())).collect(Collectors.toList());

        // The order of iteration over edges is not deterministic in BFS though all of same depth should be processed after eachother so the resulting data has to be the same
        assertReflectionEquals(expectedAsString, result, ReflectionComparatorMode.LENIENT_ORDER);
    }

    @Test
    public void testSimpleBFSWeightedReverse() throws Exception {
        Graph graph = new GraphList();
        readGraph("simple_graph_weighted.csv", graph);
        List<String> expectedAsString = asList("Bern:0", "Zürich:107");
        List<SearchResult> results = GraphSearch.breadthFirstSearch(graph, "Bern");
        List<String> result = results.stream().map(res -> String.format("%s:%d", res.getNode().getValue(), res.getCost())).collect(Collectors.toList());

        // The order of iteration over edges is not deterministic in BFS though all of same depth should be processed after eachother so the resulting data has to be the same
        assertReflectionEquals(expectedAsString, result, ReflectionComparatorMode.LENIENT_ORDER);
    }

    @Test
    public void testGraphFingeruebung() throws Exception {
        Graph graph = new GraphList();
        readGraph("graph_fingerübung.csv", graph);
        List<String> expectedAsString = asList("1:0", "3:1", "4:1", "7:1", "6:2", "8:2", "9:2", "10:3");

        List<SearchResult> results = GraphSearch.breadthFirstSearch(graph, "1");
        List<String> result = results.stream().map(res -> String.format("%s:%d", res.getNode().getValue(), res.getCost())).collect(Collectors.toList());

        result.forEach(System.out::println);
        // The order of iteration over edges is not deterministic in BFS though all of same depth should be processed after eachother so the resulting data has to be the same
        assertReflectionEquals(expectedAsString, result, ReflectionComparatorMode.LENIENT_ORDER);
    }

    private void readGraph(String name, Graph graph) throws Exception {
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        URL path = classloader.getResource(name);
        assertNotNull(path);
        File file = new File(path.toURI());
        graph.readFromFile(file);
    }
}
