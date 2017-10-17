package ch.isageek.ads.p5.list;

import org.junit.Test;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNull;

public class NodeTest {

    @Test
    public void testAddEdge() {
        NodeL a = new NodeL("a");
        NodeL b = new NodeL("b");
        EdgeL e = new EdgeL(b, 1);


        a.addEdge(e);

        assertEquals(1, a.getEdges().size());
    }

    @Test
    public void testGetEdge() {
        NodeL a = new NodeL("a");
        NodeL b = new NodeL("b");
        EdgeL e = new EdgeL(b, 1);


        a.addEdge(e);

        assertEquals(e, a.getEdgeTo(b));
    }

    @Test
    public void testGetNonexistingEdge() {
        NodeL a = new NodeL("a");
        NodeL b = new NodeL("b");
        EdgeL e = new EdgeL(b, 1);


        a.addEdge(e);

        assertNull(a.getEdgeTo(a));
    }

    @Test
    public void testRemoveEdge() {
        NodeL a = new NodeL("a");
        NodeL b = new NodeL("b");
        EdgeL e = new EdgeL(b, 1);

        a.addEdge(e);

        a.removeEdgeTo(b);

        assertEquals(0, a.getEdges().size());
        assertNull(a.getEdgeTo(b));
    }

    @Test
    public void testAddRemoveMultipleEdges() {
        NodeL a = new NodeL("a");
        NodeL b = new NodeL("b");
        NodeL c = new NodeL("c");
        EdgeL e1 = new EdgeL(b, 1);
        EdgeL e2 = new EdgeL(c, 1);

        a.addEdge(e1);
        a.addEdge(e2);

        a.removeEdgeTo(b);

        assertEquals(1, a.getEdges().size());
        assertEquals(e2, a.getEdgeTo(c));
    }
}
