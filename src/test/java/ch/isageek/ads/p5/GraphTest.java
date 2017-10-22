package ch.isageek.ads.p5;

import ch.isageek.ads.p5.exception.GraphParseException;
import ch.isageek.ads.p5.impl.GraphList;
import ch.isageek.ads.p5.impl.GraphHashmapMatrix;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.unitils.reflectionassert.ReflectionComparatorMode;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;

import static java.util.Arrays.asList;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import static org.unitils.reflectionassert.ReflectionAssert.assertReflectionEquals;

@RunWith(Parameterized.class)
public class GraphTest {

    private static final String A = "a";
    private static final String B = "b";
    private Graph graph;
    private Class cls;

    private File tmpFile = null;

    public GraphTest(Class cls) {
        this.cls = cls;
    }

    @Parameterized.Parameters(name = "{0}")
    public static Collection<Class> getClasses() {
        return asList(GraphHashmapMatrix.class, GraphList.class);
    }

    @Before
    public void setup() throws IllegalAccessException, InstantiationException {
        graph = (Graph)cls.newInstance();
    }

    @After
    public void tearDown() {
        if (tmpFile != null) {
            tmpFile.delete();
        }
    }

    @Test
    public void testSimpleGraph() throws Exception {
        Node a = graph.addNode(A);
        Node b = graph.addNode(B);

        graph.addEdge(A, B, 1);

        assertEquals(1, graph.getNumberOfEdges());
        assertEquals(2, graph.getNumberOfNodes());

        List<Edge> edges = graph.getEdgesFor(A);
        assertEquals(1, edges.size());
        assertEquals(b.getValue(), edges.get(0).getDestination().getValue());
        assertEquals(1, edges.get(0).getCost());

        assertReflectionEquals(asList(a, b), graph.getNodes());

    }

    @Test(expected = NoSuchElementException.class)
    public void testAddEdgeToNonexistingNodeDest() throws Exception {
        graph.addNode(A);
        graph.addEdge(A, B, 1);
    }

    @Test(expected = NoSuchElementException.class)
    public void testAddEdgeToNonexistingNodeSource() throws Exception {
        graph.addNode(B);
        graph.addEdge(A, B, 1);
    }

    @Test
    public void testRemoveEdgeSimpleGraph() throws Exception {
        Node a = graph.addNode(A);
        Node b = graph.addNode(B);

        graph.addEdge(A, B, 1);
        graph.addEdge(B, A, 1);

        assertEquals(2, graph.getNumberOfEdges());
        assertEquals(2, graph.getNumberOfNodes());
        assertEquals(1, graph.getEdgesFor(A).size());
        assertEquals(1, graph.getEdgesFor(B).size());

        graph.removeEdge(B, A);

        assertEquals(1, graph.getNumberOfEdges());

        List<Edge> edges = graph.getEdgesFor(A);
        assertEquals(1, edges.size());
        assertEquals(b.getValue(), edges.get(0).getDestination().getValue());
        assertEquals(1, edges.get(0).getCost());

        assertReflectionEquals(asList(a, b), graph.getNodes());

    }

    @Test
    public void testRemoveNodeSimpleGraph() throws Exception {

        Node a = graph.addNode(A);
        graph.addNode(B);

        graph.addEdge(A, B, 1);
        graph.addEdge(B, A, 1);

        graph.removeNode(B);

        assertEquals(0, graph.getNumberOfEdges());
        assertEquals(1, graph.getNumberOfNodes());

        List<Edge> edges = graph.getEdgesFor(A);
        assertEquals(0, edges.size());

        assertReflectionEquals(Collections.singletonList(a), graph.getNodes());
    }

    @Test
    public void testReadEdgeListGraph() throws Exception {
        readGraph("simple_graph_edgelist.txt");

        assertEquals(2, graph.getNumberOfNodes());
        assertEquals(2, graph.getNumberOfEdges());
        assertEquals(1, graph.getEdgesFor("Zürich").size());
        assertEquals(1, graph.getEdgesFor("Bern").size());

        List<Edge> edges = graph.getEdgesFor("Zürich");
        assertEquals(1, edges.size());
        assertEquals("Bern", edges.get(0).getDestination().getValue());
        assertEquals(1, edges.get(0).getCost());

        List<Edge> edges2 = graph.getEdgesFor("Bern");
        assertEquals(1, edges2.size());
        assertEquals("Zürich", edges2.get(0).getDestination().getValue());
        assertEquals(1, edges2.get(0).getCost());
    }

    @Test
    public void testReadEdgeListGraphCsv() throws Exception {
        readGraph("simple_graph.csv");

        assertEquals(2, graph.getNumberOfNodes());
        assertEquals(2, graph.getNumberOfEdges());
        assertEquals(1, graph.getEdgesFor("Zürich").size());
        assertEquals(1, graph.getEdgesFor("Bern").size());

        List<Edge> edges = graph.getEdgesFor("Zürich");
        assertEquals(1, edges.size());
        assertEquals("Bern", edges.get(0).getDestination().getValue());
        assertEquals(1, edges.get(0).getCost());

        List<Edge> edges2 = graph.getEdgesFor("Bern");
        assertEquals(1, edges2.size());
        assertEquals("Zürich", edges2.get(0).getDestination().getValue());
        assertEquals(1, edges2.get(0).getCost());
    }

    @Test
    public void testReadEdgeListGraphCsvBig() throws Exception {
        readGraph("graph_fingerübung.csv");

        assertEquals(10, graph.getNumberOfNodes());
        assertEquals(13, graph.getNumberOfEdges());
        assertEquals(2, graph.getEdgesFor("2").size());
        assertEquals(3, graph.getEdgesFor("1").size());

        List<Edge> edges = graph.getEdgesFor("2");
        assertReflectionEquals(
                asList("8", "5"),
                edges.stream().map(edge -> edge.getDestination().getValue()).collect(Collectors.toList())
        , ReflectionComparatorMode.LENIENT_ORDER);
        assertEquals(1, edges.get(0).getCost());
        assertEquals(1, edges.get(1).getCost());

        List<Edge> edges2 = graph.getEdgesFor("1");
        assertReflectionEquals(
                asList("3", "4", "7"),
               edges2.stream().map(edge -> edge.getDestination().getValue()).collect(Collectors.toList())
                , ReflectionComparatorMode.LENIENT_ORDER);

        assertEquals(1, edges2.get(0).getCost());
        assertEquals(1, edges2.get(1).getCost());
        assertEquals(1, edges2.get(2).getCost());
    }

    @Test
    public void testReadEdgeListGraphWeighted() throws Exception {
        readGraph("simple_graph_edgelist_weighted.txt");

        assertEquals(2, graph.getNumberOfNodes());
        assertEquals(2, graph.getNumberOfEdges());
        assertEquals(1, graph.getEdgesFor("Zürich").size());
        assertEquals(1, graph.getEdgesFor("Bern").size());

        List<Edge> edges = graph.getEdgesFor("Zürich");
        assertEquals(1, edges.size());
        assertEquals("Bern", edges.get(0).getDestination().getValue());
        assertEquals(110, edges.get(0).getCost());

        List<Edge> edges2 = graph.getEdgesFor("Bern");
        assertEquals(1, edges.size());
        assertEquals("Zürich", edges2.get(0).getDestination().getValue());
        assertEquals(107, edges2.get(0).getCost());
    }

    @Test
    public void testReadEdgeListGraphWeightedCSV() throws Exception {
        readGraph("simple_graph_weighted.csv");

        assertEquals(2, graph.getNumberOfNodes());
        assertEquals(2, graph.getNumberOfEdges());
        assertEquals(1, graph.getEdgesFor("Zürich").size());
        assertEquals(1, graph.getEdgesFor("Bern").size());

        List<Edge> edges = graph.getEdgesFor("Zürich");
        assertEquals(1, edges.size());
        assertEquals("Bern", edges.get(0).getDestination().getValue());
        assertEquals(110, edges.get(0).getCost());

        List<Edge> edges2 = graph.getEdgesFor("Bern");
        assertEquals(1, edges.size());
        assertEquals("Zürich", edges2.get(0).getDestination().getValue());
        assertEquals(107, edges2.get(0).getCost());
    }

    @Test(expected = GraphParseException.class)
    public void testReadGraphEmptyFile() throws Exception {
        writeToTmpFile("");
        graph.readFromFile(tmpFile);
    }

    @Test(expected = GraphParseException.class)
    public void testReadGraphOnlyNodeCount() throws Exception {
        writeToTmpFile("1");
        graph.readFromFile(tmpFile);
    }

    @Test(expected = GraphParseException.class)
    public void testReadGraphOnlySkippedEdgeCount() throws Exception {
        writeToTmpFile("2,Zürich,Bern");
        graph.readFromFile(tmpFile);
    }

    @Test(expected = GraphParseException.class)
    public void testReadGraphUnevenEdges() throws Exception {
        writeToTmpFile("2,2,Zürich,Bern,Zürich");
        graph.readFromFile(tmpFile);
    }

    @Test(expected = GraphParseException.class)
    public void testReadWeightedGraphOnlyNodeCount() throws Exception {
        writeToTmpFile("W:1");
        graph.readFromFile(tmpFile);
    }

    @Test(expected = GraphParseException.class)
    public void testReadWeightedGraphOnlySkippedEdgeCount() throws Exception {
        writeToTmpFile("W:2,Zürich,Bern");
        graph.readFromFile(tmpFile);
    }

    @Test(expected = GraphParseException.class)
    public void testReadWeightedGraphUnevenEdges() throws Exception {
        writeToTmpFile("W:2,2,Zürich,Bern,Zürich");
        graph.readFromFile(tmpFile);
    }

    private void writeToTmpFile(String str) throws IOException {
        tmpFile = File.createTempFile("graph", ".txt");
        BufferedWriter out = new BufferedWriter(new FileWriter(tmpFile));
        out.write(str);
        out.close();
    }

    private void readGraph(String name) throws Exception {
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        URL path = classloader.getResource(name);
        assertNotNull(path);
        File file = new File(path.toURI());
        graph.readFromFile(file);
    }
}
