package ch.isageek.ads.p6;

import ch.isageek.ads.p5.exception.GraphParseException;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

import static org.junit.Assert.assertEquals;

public class Exercise4Test {
    private static final String FILENAME = "swiss_cities.csv";

    private Exercise4 exercise;

    @Before
    public void setup() throws Exception {
        exercise = new Exercise4(loadFile(FILENAME));
    }

    @Test
    public void testTotalDistance() throws Exception {
        assertEquals(25 + 126 + 277 + 54 + 121 + 16 + 155 + 363 + 206 + 152 + 146 + 97 + 102 + 41, exercise.computeTotalDistance());
    }

    private File loadFile(String name) throws Exception {
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        URL path = classloader.getResource(FILENAME);
        if (path == null) {
            throw new FileNotFoundException(FILENAME);
        }

        return new File(path.toURI());
    }
}
