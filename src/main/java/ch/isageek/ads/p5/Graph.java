package ch.isageek.ads.p5;

import java.io.File;
import java.util.List;
import java.util.NoSuchElementException;

public interface Graph {

    enum FileType {
        EDGELIST, NODELIST
    }

    /**
     * Constructs a graph reading the file from the path.
     *
     * @param file the file to load
     * @param type The type of representation of the file, either an edgelist or a nodelist
     */
    void readFromFile(File file, FileType type);

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
