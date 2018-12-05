package ch.isageek.ads.p6;

import ch.isageek.ads.p5.Node;
import org.junit.Test;
import org.unitils.reflectionassert.ReflectionComparatorMode;

import java.util.stream.Collectors;

import static java.util.Arrays.asList;
import static junit.framework.TestCase.assertEquals;
import static org.unitils.reflectionassert.ReflectionAssert.assertReflectionEquals;

public class UndirectedGraphTest {

    @Test
    public void testSimple() throws Exception {
        UndirectedGraphList graph = new UndirectedGraphList();

        graph.addNode("a");
        graph.addNode("b");

        graph.addEdge("a", "b", 20);

        assertEquals(2, graph.getNumberOfNodes());
        assertEquals(1, graph.getNumberOfEdges());

        Node a = graph.getNode("a");
        assertEquals(1, a.getEdges().size());
        assertEquals("b", a.getEdges().get(0).getDestination().getValue());

        Node b = graph.getNode("b");
        assertEquals(1, b.getEdges().size());
        assertEquals("a", b.getEdges().get(0).getDestination().getValue());
    }

    @Test
    public void testBiggerGraph() throws Exception {
        UndirectedGraphList graph = new UndirectedGraphList();

        graph.addNode("a");
        graph.addNode("b");
        graph.addNode("c");

        graph.addEdge("a", "b", 20);
        graph.addEdge("a", "c", 20);

        assertEquals(3, graph.getNumberOfNodes());
        assertEquals(2, graph.getNumberOfEdges());

        Node a = graph.getNode("a");
        assertEquals(2, a.getEdges().size());
        assertReflectionEquals(
                asList("b", "c"),
                a.getEdges().stream().map(edge -> edge.getDestination().getValue()).collect(Collectors.toList()),
                ReflectionComparatorMode.LENIENT_ORDER
        );

        Node b = graph.getNode("b");
        assertEquals(1, b.getEdges().size());
        assertEquals("a", b.getEdges().get(0).getDestination().getValue());


        Node c = graph.getNode("c");
        assertEquals(1, c.getEdges().size());
        assertEquals("a", c.getEdges().get(0).getDestination().getValue());
    }
}
