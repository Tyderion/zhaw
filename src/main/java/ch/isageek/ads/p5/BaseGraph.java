package ch.isageek.ads.p5;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public abstract class BaseGraph implements Graph {
    @Override
    public void readFromFile(File file, FileType type) throws IOException {
        List<String> lines = Files.readAllLines(file.toPath(), Charset.forName("UTF-8"));
        if (lines.size() > 1) {
            System.err.println("Both NodeList and EdgeList representations are on one line. Reading first line as graph");
        }
        if (lines.size() < 1) {
            System.err.println("One line of text is required for a graph definition");
            throw new IOException("File is empty");
        }
        String[] graphInformation = lines.get(0).split(",");
        System.out.println(Arrays.toString(graphInformation));
        Set<String> nodes = new HashSet<>();
        Set<Edge> edges = new HashSet<>();
    }

    private static class Edge {
        public String start;
        public String end;

        public Edge(String start, String end) {
            this.start = start;
            this.end = end;
        }
    }
}
