package ch.isageek.ads.p3;

import java.util.ArrayList;

public class BinaryTree<T extends Comparable<T>> {

    private TreeNode<T> root;

    public BinaryTree(T value) {
        root = new TreeNode<>(value);
    }

    public BinaryTree() {
        root = new TreeNode<>(null);
    }

    public ArrayList<Integer> traversePostorder() {
        return new ArrayList<>();
    }

    public ArrayList<Integer> traverseLevelorder() {
        return new ArrayList<>();
    }

    public ArrayList<Integer> traversePreorder() {
        return new ArrayList<>();
    }

    public ArrayList<Integer> traverseInorder() {
        return new ArrayList<>();
    }

    public TreeNode<T> getRoot() {
        return root;
    }

    public void setRoot(TreeNode<T> root) {
        this.root = root;
    }
}
