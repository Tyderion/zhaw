package ch.isageek.ads.p5.list;

import ch.isageek.ads.p5.Node;
import org.junit.Test;

import java.util.NoSuchElementException;

import static java.util.Arrays.asList;
import static junit.framework.TestCase.assertEquals;
import static org.unitils.reflectionassert.ReflectionAssert.assertReflectionEquals;

public class GraphListTest {

    @Test
    public void testSimpleGraph() throws Exception {
        GraphList g = new GraphList(2);

        Node a = g.addNode("a");
        Node b = g.addNode("b");

        g.addEdge("a", "b", 1);

        NodeL node = g.getNode("a");

        assertEquals(1, node.getEdges().size());
        assertEquals(1, g.getNumberOfEdges());
        assertEquals(2, g.getNumberOfNodes());

        assertEquals(g.getNode("b"), node.getEdges().get(0).getDestination());
        assertEquals(1, node.getEdges().get(0).getCost());

        assertReflectionEquals(asList(a, b), g.getNodes());
    }

    @Test(expected = NoSuchElementException.class)
    public void testAddEdgeToNonexistingNode() throws Exception {
        GraphList g = new GraphList(1);

        g.addNode("a");
        g.addEdge("a", "b", 1);
    }

    @Test
    public void testRemoveEdgeSimpleGraph() throws Exception {
        GraphList g = new GraphList(2);

        NodeL a = g.addNode("a");
        NodeL b = g.addNode("b");

        g.addEdge("a", "b", 1);
        g.addEdge("b", "a", 1);

        assertEquals(1, a.getEdges().size());
        assertEquals(1, b.getEdges().size());
        assertEquals(2, g.getNumberOfEdges());
        assertEquals(2, g.getNumberOfNodes());

        g.removeEdge("b", "a");

        assertEquals(1, a.getEdges().size());

        assertEquals(g.getNode("b"), a.getEdges().get(0).getDestination());
        assertEquals(1, a.getEdges().get(0).getCost());

        assertReflectionEquals(asList(a, b), g.getNodes());
    }

    @Test
    public void testRemoveNodeSimpleGraph() throws Exception {
        GraphList g = new GraphList(2);

        NodeL a = g.addNode("a");
        g.addNode("b");

        g.addEdge("a", "b", 1);
        g.addEdge("b", "a", 1);

        g.removeNode("b");

        NodeL node = g.getNode("a");

        assertEquals(0, node.getEdges().size());
    }


}
