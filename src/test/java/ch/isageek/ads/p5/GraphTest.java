package ch.isageek.ads.p5;

import ch.isageek.ads.p5.list.GraphList;
import ch.isageek.ads.p5.matrix.GraphMatrix;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;

import static java.util.Arrays.asList;
import static junit.framework.TestCase.assertEquals;
import static org.unitils.reflectionassert.ReflectionAssert.assertReflectionEquals;

@RunWith(Parameterized.class)
public class GraphTest {

    private static final String A = "a";
    private static final String B = "b";
    private Graph graph;
    private Class cls;

    public GraphTest(Class cls) {
        this.cls = cls;
    }

    @Parameterized.Parameters(name = "{0}")
    public static Collection<Class> getClasses() {
        return asList(GraphMatrix.class, GraphList.class);
    }

    @Before
    public void setup() throws IllegalAccessException, InstantiationException {
        graph = (Graph)cls.newInstance();
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

        assertReflectionEquals(asList(a.getValue(), b.getValue()), graph.getNodes().stream().map(Node::getValue).collect(Collectors.toList()));

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

        assertReflectionEquals(asList(a.getValue(), b.getValue()), graph.getNodes().stream().map(Node::getValue).collect(Collectors.toList()));
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

        assertReflectionEquals(Collections.singletonList(a.getValue()), graph.getNodes().stream().map(Node::getValue).collect(Collectors.toList()));
    }


}
