package ch.isageek.ads.p5;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public abstract class BaseGraph implements Graph {

    @Override
    public void readFromFile(File file) throws IOException, GraphParseException {
        parse(getParseInformation(file));
    }

    private void parse(ParseInformation parseInformation) throws IOException, GraphParseException {
        Set<String> nodes = new HashSet<>(parseInformation.numberOfNodes);
        Set<Edge> edges = new HashSet<>(parseInformation.numberOfEdges);
        switch (parseInformation.type) {
            case EDGELIST:
                if (parseInformation.line.size() % 2 != 0) {
                    System.err.println("Weightless edgelist must contain an even number of entries");
                    throw new GraphParseException("Weightless edgelist must contain an even number of entries");
                }
                for (int i = 0; i < parseInformation.line.size(); i = i+2) {
                    String first = parseInformation.line.get(i);
                    String second = parseInformation.line.get(i+1);
                    nodes.add(first);
                    nodes.add(second);
                    edges.add(new Edge(first, second));
                }
                break;
            case EDGELIST_WEIGHTED:
                if (parseInformation.line.size() % 3 != 0) {
                    System.err.println("Weighted edgelist must contain 3 entries for each edge");
                    throw new GraphParseException("Weighted edgelist must contain 3 entries for each edge");
                }
                for (int i = 0; i < parseInformation.line.size(); i = i+3) {
                    String first = parseInformation.line.get(i);
                    String second = parseInformation.line.get(i+1);
                    int cost;
                    try {
                        cost = Integer.parseInt(parseInformation.line.get(i+2));
                    } catch (NumberFormatException e) {
                        System.err.println("Weights must be integers");
                        throw new GraphParseException(String.format("All weights must be integers but %s is not.", parseInformation.line.get(i+2)));
                    }
                    nodes.add(first);
                    nodes.add(second);
                    edges.add(new Edge(first, second, cost));
                }
        }

        nodes.forEach(label -> {
            try {
                addNode(label);
            } catch (NodeAlreadyDefinedException e) {
                // Cannot happen, a set does not contain duplicates
            }
        });

        edges.forEach(edge -> addEdge(edge.start, edge.end, edge.cost));
    }

    private ParseInformation getParseInformation(File file) throws IOException, GraphParseException {
        List<String> lines = Files.readAllLines(file.toPath(), Charset.forName("UTF-8"));

        if (lines.size() < 1) {
            System.err.println("One line of text is required for a graph definition");
            throw new GraphParseException("File is empty");
        }

        String content;
        if (lines.size() > 1) {
            // Assuming csv
            System.out.println("File contains multiple lines, assuming csv representation");
            // We transform the csv to a "fake" edgelist, NodeCount and EdgeCount is not necessary to read the graph so we set it to an arbitrary number (1)
            boolean isWeighted = lines.get(1).split(",").length  == 3;
            content = (isWeighted ? "W:" : "") + "1,1,"+ lines.stream().collect(Collectors.joining(","));
        } else {
            content = lines.get(0);
        }

        FileType type = content.startsWith("W:") ? FileType.EDGELIST_WEIGHTED : FileType.EDGELIST;
        String[] graphInformation = content.replace("W:", "").split(",");
        if (graphInformation.length < 2) {
            System.err.println("Edgelist too short, needs at least 2 entries specifying nodecount and edgecount");
            throw new GraphParseException("Edgelist too short, needs at least 2 entries specifying nodecount and edgecount");
        }
        int numberOfNodes;
        int numberOfEdges;
        try {
            numberOfNodes = Integer.parseInt(graphInformation[0]);
            numberOfEdges = Integer.parseInt(graphInformation[1]);
        } catch (NumberFormatException e) {
            System.err.println("File does not start with 2 integers");
            throw new GraphParseException("Could not parse either node count or edge count.");
        }
        return new ParseInformation(numberOfNodes, numberOfEdges, Arrays.stream(graphInformation).skip(2).collect(Collectors.toList()), type);
    }

    private static class Edge {
        String start;
        String end;
        int cost;

        Edge(String start, String end) {
            this(start, end, 1);
        }

        Edge(String start, String end, int cost) {
            this.start = start;
            this.end = end;
            this.cost = cost;
        }
    }

    private static class ParseInformation {
        int numberOfNodes;
        int numberOfEdges;
        List<String> line;
        FileType type;

        ParseInformation(int numberOfNodes, int numberOfEdges,List<String> line, FileType type) {
            this.numberOfNodes = numberOfNodes;
            this.numberOfEdges = numberOfEdges;
            this.line = line;
            this.type = type;
        }
    }
}
