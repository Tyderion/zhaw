package ch.isageek.ads.p3;

import org.junit.Test;

import java.util.ArrayList;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.unitils.reflectionassert.ReflectionAssert.assertReflectionEquals;

public class BinarySearchTreeTest {


    @Test
    public void testSimpleTree() throws Exception {
        BinarySearchTree<Integer> tree = new BinarySearchTree<>();
        tree.add(1);
        tree.add(0);
        tree.add(2);
        ArrayList<Integer> inorder = tree.traverseInorder();

        assertReflectionEquals(asList(0, 1, 2), inorder);
    }


    @Test
    public void testFindElementInSmallTree() throws Exception {
        Competitor c1 = createCompetitor("A");
        Competitor c2 = createCompetitor("B");
        Competitor c3 = createCompetitor("C");
        Competitor searchValue = createCompetitor("B", "NOT EMPTY");

        BinarySearchTree<Competitor> tree = new BinarySearchTree<>();
        tree.add(c2);
        tree.add(c1);
        tree.add(c3);
        Competitor foundElement = tree.find(searchValue);

        assertNotNull(foundElement);
        assertEquals(c2, foundElement);
        assertNotEquals(searchValue.getCity(), foundElement.getCity());
    }

    @Test
    public void testFindElementInBiggerTree() throws Exception {
        Competitor c1 = createCompetitor("A");
        Competitor c2 = createCompetitor("B");
        Competitor c3 = createCompetitor("C");
        Competitor c4 = createCompetitor("D");
        Competitor c5 = createCompetitor("E");
        Competitor c6 = createCompetitor("F");
        Competitor c7 = createCompetitor("G");
        Competitor c8 = createCompetitor("H");
        Competitor searchValue = createCompetitor("E", "NOT EMPTY");

        BinarySearchTree<Competitor> tree = new BinarySearchTree<>();
        tree.add(c2);
        tree.add(c1);
        tree.add(c8);
        tree.add(c4);
        tree.add(c5);
        tree.add(c3);
        tree.add(c7);
        tree.add(c6);
        Competitor foundElement = tree.find(searchValue);

        assertNotNull(foundElement);
        assertEquals(c5, foundElement);
        assertNotEquals(searchValue.getCity(), foundElement.getCity());
    }


    private Competitor createCompetitor(final String lastname) throws Exception {
        return createCompetitor(lastname, "");
    }
    private Competitor createCompetitor(final String lastname, String city) throws Exception {
        return new Competitor(1, "", lastname, 1999, city, "12:15:12.132");
    }
}
