package ch.isageek.ads.p3;

import java.util.ArrayList;

public class BinaryTree<T extends Comparable<T>> {

    private TreeNode<T> root;

    public BinaryTree(final T value) {
        root = new TreeNode<>(value);
    }

    public BinaryTree() {
    }

    public ArrayList<T> traversePostorder() {
        return traverse(root, (node, left, right) -> {
            ArrayList<T> result = new ArrayList<>();
            result.addAll(left);
            result.addAll(right);
            result.add(node);
            return result;
        });
    }

    public ArrayList<T> traverseLevelorder() {
        return new ArrayList<>();
    }

    public ArrayList<T> traversePreorder() {
        return traverse(root, (node, left, right) -> {
            ArrayList<T> result = new ArrayList<>();
            result.add(node);
            result.addAll(left);
            result.addAll(right);
            return result;
        });
    }

    public ArrayList<T> traverseInorder() {
        return traverse(root, (node, left, right) -> {
            ArrayList<T> result = new ArrayList<>();
            result.addAll(left);
            result.add(node);
            result.addAll(right);
            return result;
        });
    }

    private ArrayList<T> traverse(final TreeNode<T> root, final TraverseOperation<T> op) {
        if (root == null) {
            return new ArrayList<>();
        }
        final TreeNode<T> left = root.getLeft();
        final TreeNode<T> right = root.getRight();
        final ArrayList<T> leftTraversal = left == null ? new ArrayList<>(0) : traverse(left, op);
        final ArrayList<T> rightTraversal = right == null ? new ArrayList<>(0) : traverse(right, op);

        return op.combine(root.getElement(), leftTraversal, rightTraversal);
    }

    public TreeNode<T> getRoot() {
        return root;
    }

    public void setRoot(final TreeNode<T> root) {
        this.root = root;
    }


    @FunctionalInterface
    interface TraverseOperation<T> {
        ArrayList<T> combine(final T node, final ArrayList<T> left, final ArrayList<T> right);
    }
}
