package ch.isageek.ads.p5.list;

import org.junit.Test;

import java.util.NoSuchElementException;

import static junit.framework.TestCase.assertEquals;

public class GraphListTest {

    @Test
    public void testSimpleGraph() throws Exception {
        GraphList g = new GraphList(2);

        g.addNode("a");
        g.addNode("b");

        g.addEdge("a", "b", 1);

        Node node = g.getNode("a");

        assertEquals(1, node.getEdges().size());

        assertEquals(g.getNode("b"), node.getEdges().get(0).getDestination());
        assertEquals(1, node.getEdges().get(0).getCost());
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

        Node a = g.addNode("a");
        Node b = g.addNode("b");

        g.addEdge("a", "b", 1);
        g.addEdge("b", "a", 1);

        assertEquals(1, a.getEdges().size());
        assertEquals(1, b.getEdges().size());

        g.removeEdge("b", "a");

        assertEquals(1, a.getEdges().size());

        assertEquals(g.getNode("b"), a.getEdges().get(0).getDestination());
        assertEquals(1, a.getEdges().get(0).getCost());
    }

    @Test
    public void testRemoveNodeSimpleGraph() throws Exception {
        GraphList g = new GraphList(2);

        Node a = g.addNode("a");
        g.addNode("b");

        g.addEdge("a", "b", 1);
        g.addEdge("b", "a", 1);

        g.removeNode("b");


        Node node = g.getNode("a");

        assertEquals(0, node.getEdges().size());
    }


}
