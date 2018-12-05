package ch.isageek.ads.p5;

import ch.isageek.ads.p5.exception.GraphParseException;
import ch.isageek.ads.p5.exception.NodeAlreadyDefinedException;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.NoSuchElementException;

public interface Graph {

    /**
     * Reads a graph from a file with a representation documented in {@link FileType}
     * @param file the file to load
     * @throws IOException if the file cannot be opened
     * @throws GraphParseException if the file cannot be parsed
     */
    void readFromFile(File file) throws IOException, GraphParseException;


    int getNumberOfNodes();

    int getNumberOfEdges();

    List<Node> getNodes();

    List<Edge> getEdgesFor(final String label);

    Node addNode(final String label) throws NodeAlreadyDefinedException;

    Node getNode(final String label) throws NoSuchElementException;

    void removeNode(final String label) throws NoSuchElementException;

    void addEdge(final String src, final String dest, final int cost);

    void removeEdge(final String src, final String dest);
}
