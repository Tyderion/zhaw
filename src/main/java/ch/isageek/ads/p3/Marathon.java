package ch.isageek.ads.p3;

import java.io.*;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.ParseException;
import java.util.stream.Stream;

public class Marathon {

    private final static String FILENAME = "zuerich_marathon_utf8.csv";

    /**
     * Loads the file 'zuerich_marathon_utf8.csv' into a {@link BinarySearchTree} as {@link Competitor}s
     * loading the file with {@link ClassLoader#getResource(String)}.
     * <p>
     * After loading the data into a tree, an inorder (e.g. sorted) traversal of the tree is printed on the console.
     *
     * @param args ignored
     */
    public static void main(String[] args) {
        BinarySearchTree<Competitor> tree = new BinarySearchTree<>();

        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        try {
            URL file = classloader.getResource(FILENAME);
            if (file != null) {
                Files.lines(Paths.get(URI.create("file://" + file.getPath())))
                        .forEach(line -> {
                            String[] parts = line.split(",");
                            try {
                                tree.add(new Competitor(
                                        Integer.valueOf(parts[0]),
                                        parts[1],
                                        parts[2],
                                        Integer.valueOf(parts[3]),
                                        parts[4],
                                        parts[5]
                                ));
                            } catch (ParseException | NullPointerException e) {
                                System.out.println(String.format("Error parsing competitor: %s", line));
                            }
                        });
            } else {
                System.out.println("File not found: " + FILENAME);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        if (tree.getRoot() != null) {
            System.out.println("Inserted all people into the search tree.");
            System.out.println("Inorder Traversal: ");
            tree.traverseInorder().forEach(System.out::println);
        }

    }
}
