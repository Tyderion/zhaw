package ch.isageek.ads.p5.exception;

public class NodeAlreadyDefinedException extends Exception {

    public NodeAlreadyDefinedException(String message) {
        super(String.format("Node %s is already defined.", message));
    }
}
