package ch.isageek.ads.p5;

public enum FileType {
    /**
     * Represents typical Edgelist representation:
     * example <code>2,2,a,b,b,a</code>
     * Definition is: NodeCount,EdgeCount,[List of edges in the form of: NodeA,NodeB]
     */
    EDGELIST,
    /**
     * Represents an Edgelist representation with additional weights:
     * example <code>W:2,2,a,b,2,b,a,1</code>
     * Definition is: W:NodeCount,EdgeCount,[List of edges in the form of: NodeA,NodeB,weight]
     */
    EDGELIST_WEIGHTED
}
