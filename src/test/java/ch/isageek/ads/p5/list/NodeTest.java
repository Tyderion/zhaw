package ch.isageek.ads.p5.list;

import org.junit.Test;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNull;

public class NodeTest {

    @Test
    public void testAddEdge() {
        Node a = new Node("a");
        Node b = new Node("b");
        Edge e = new Edge(b, 1);


        a.addEdge(e);

        assertEquals(1, a.getEdges().size());
    }

    @Test
    public void testGetEdge() {
        Node a = new Node("a");
        Node b = new Node("b");
        Edge e = new Edge(b, 1);


        a.addEdge(e);

        assertEquals(e, a.getEdgeTo(b));
    }

    @Test
    public void testGetNonexistingEdge() {
        Node a = new Node("a");
        Node b = new Node("b");
        Edge e = new Edge(b, 1);


        a.addEdge(e);

        assertNull(a.getEdgeTo(a));
    }

    @Test
    public void testRemoveEdge() {
        Node a = new Node("a");
        Node b = new Node("b");
        Edge e = new Edge(b, 1);

        a.addEdge(e);

        a.removeEdgeTo(b);

        assertEquals(0, a.getEdges().size());
        assertNull(a.getEdgeTo(b));
    }

    @Test
    public void testAddRemoveMultipleEdges() {
        Node a = new Node("a");
        Node b = new Node("b");
        Node c = new Node("c");
        Edge e1 = new Edge(b, 1);
        Edge e2 = new Edge(c, 1);

        a.addEdge(e1);
        a.addEdge(e2);

        a.removeEdgeTo(b);

        assertEquals(1, a.getEdges().size());
        assertEquals(e2, a.getEdgeTo(c));
    }
}
