package ch.isageek.ads.p3;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class CompetitorTest {

    @Test
    public void testToString() throws Exception {
        final String result = "1: 12:15:12.132 - Max Müller (1999, Zürich)";
        Competitor competitor = createCompetitor("Max", "Müller");

        assertEquals(result, competitor.toString());
    }

    @Test
    public void testCompareToLessLastName() throws Exception {
        Competitor c1 = createCompetitor("Max", "Meier");
        Competitor c2 = createCompetitor("Max", "Müller");

        int result = c1.compareTo(c2);

        assertTrue(result < 0);
    }

    @Test
    public void testCompareToLessFirstName() throws Exception {
        Competitor c1 = createCompetitor("Max", "Müller");
        Competitor c2 = createCompetitor("Stephan", "Müller");

        int result = c1.compareTo(c2);

        assertTrue(result < 0);
    }

    @Test
    public void testCompareToMoreFirstName() throws Exception {
        Competitor c1 = createCompetitor("Max", "Müller");
        Competitor c2 = createCompetitor("Stephan", "Müller");

        int result = c2.compareTo(c1);

        assertTrue(result > 0);
    }

    @Test
    public void testCompareToMoreLastName() throws Exception {
        Competitor c1 = createCompetitor("Max", "Meier");
        Competitor c2 = createCompetitor("Max", "Müller");

        int result = c2.compareTo(c1);

        assertTrue(result > 0);
    }

    @Test
    public void testCompareToEquals() throws Exception {
        Competitor c1 = createCompetitor("Max", "Müller");
        Competitor c2 = createCompetitor("Max", "Müller");

        int result = c1.compareTo(c2);

        assertEquals(0, result);
    }

    @Test
    public void testEquals() throws Exception {
        Competitor c1 = createCompetitor("Max", "Müller");
        Competitor c2 = createCompetitor("Max", "Müller");

        assertTrue(c1.equals(c2));
    }

    @Test
    public void testEqualsNotEquals() throws Exception {
        Competitor c1 = createCompetitor("Moritz", "Müller");
        Competitor c2 = createCompetitor("Max", "Müller");

        assertFalse(c1.equals(c2));
    }

    private Competitor createCompetitor(final String firstname, final String lastname) throws Exception {
        return new Competitor(1, firstname, lastname, 1999, "Zürich", "12:15:12.132");
    }
}
