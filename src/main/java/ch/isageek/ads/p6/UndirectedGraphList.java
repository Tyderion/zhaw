package ch.isageek.ads.p6;

import ch.isageek.ads.p5.impl.GraphList;

/**
 * We can imagine an undirected graph to be a directed graph, where we add/remove edges only in pairs.
 * e.g. instead of adding edge (x,y) we add edges (x,y) and (y,x)
 */
public class UndirectedGraphList extends GraphList {

    @Override
    public void addEdge(String src, String dest, int cost) {
        super.addEdge(src, dest, cost);
        super.addEdge(dest, src, cost);
    }

    @Override
    public void removeEdge(String src, String dest) {
        super.removeEdge(src, dest);
        super.removeEdge(dest, src);
    }

    @Override
    public int getNumberOfEdges() {
        return super.getNumberOfEdges() / 2;
    }
}
