package ch.isageek.ads.p6;

import ch.isageek.ads.p5.Graph;
import ch.isageek.ads.p5.Node;
import ch.isageek.ads.p5.exception.NodeAlreadyDefinedException;
import ch.isageek.ads.p5.impl.GraphList;
import org.junit.Test;

import java.util.NoSuchElementException;

import static junit.framework.TestCase.assertEquals;

public class DijikstraTest {

    private static final Class<GraphList> GRAPH_CLASS = GraphList.class;

    @Test
    public void testOnlyTwoNodes() throws Exception {
        final String startNode = "a";
        Graph graph = graphWithTwoNodes();
        Dijikstra dijikstra = new Dijikstra<>(graph, GRAPH_CLASS);
        Dijikstra.Path result = dijikstra.computePath(startNode, "b");
        assertEquals(20, result.getLength());

        Node start = result.getGraph().getNode(startNode);
        assertEquals(1, start.getEdges().size());
        assertEquals(startNode, start.getValue());
        assertEquals(20, start.getEdges().get(0).getCost());
        assertEquals("b", start.getEdges().get(0).getDestination().getValue());
        assertEquals(0, start.getEdges().get(0).getDestination().getEdges().size());
    }

    @Test(expected = NoSuchElementException.class)
    public void testNonexistingNode1() throws Exception {
        Graph graph = graphWithTwoNodes();
        Dijikstra dijikstra = new Dijikstra<>(graph, GRAPH_CLASS);
        dijikstra.computePath("a", "c");
    }

    @Test(expected = NoSuchElementException.class)
    public void testNonexistingNode2() throws Exception {
        Graph graph = graphWithTwoNodes();
        Dijikstra dijikstra = new Dijikstra<>(graph, GRAPH_CLASS);
        dijikstra.computePath("c", "b");
    }

    @Test
    public void testThreeNodesMultiPath() throws Exception {
        final String startNode = "a";
        Graph graph = graphWithThreeNodes();
        Dijikstra dijikstra = new Dijikstra<>(graph, GRAPH_CLASS);
        Dijikstra.Path result = dijikstra.computePath(startNode, "c");
        assertEquals(40, result.getLength());

        Node start = result.getGraph().getNode(startNode);
        assertEquals(1, start.getEdges().size());
        assertEquals(startNode, start.getValue());
        assertEquals(20, start.getEdges().get(0).getCost());

        Node b = start.getEdges().get(0).getDestination();
        assertEquals("b", b.getValue());
        assertEquals(1, b.getEdges().size());
        assertEquals(20, b.getEdges().get(0).getCost());

        Node c = b.getEdges().get(0).getDestination();
        assertEquals("c", c.getValue());
        assertEquals(0, c.getEdges().size());
    }



    private Graph graphWithTwoNodes() throws NodeAlreadyDefinedException {
        Graph graph = new GraphList(2);
        graph.addNode("a");
        graph.addNode("b");
        graph.addEdge("a", "b", 20);
        return graph;
    }

    private Graph graphWithThreeNodes() throws NodeAlreadyDefinedException {
        Graph graph = new GraphList(2);
        graph.addNode("a");
        graph.addNode("b");
        graph.addNode("c");
        graph.addEdge("a", "b", 20);
        graph.addEdge("a", "c", 50);
        graph.addEdge("b", "c", 20);
        return graph;
    }
}
