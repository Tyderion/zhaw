package ch.isageek.ads.p3;

import java.util.ArrayList;
import java.util.function.BiFunction;
import java.util.function.Function;

public class BinaryTree<T extends Comparable<T>> {

    private TreeNode<T> root;

    public BinaryTree(T value) {
        root = new TreeNode<>(value);
    }

    public BinaryTree() {
        root = new TreeNode<>(null);
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

    private ArrayList<T> traverse(TreeNode<T> root, TraverseOperation<T> op) {
        if (root.getElement() == null) {
            return new ArrayList<>();
        }
        TreeNode<T> left = root.getLeft();
        TreeNode<T> right = root.getRight();
        ArrayList<T> leftTraversal = left == null ? new ArrayList<>(0) : traverse(left, op);
        ArrayList<T> rightTraversal = right == null ? new ArrayList<>(0) : traverse(right, op);

        return op.combine(root.getElement(), leftTraversal, rightTraversal);
    }

    public TreeNode<T> getRoot() {
        return root;
    }

    public void setRoot(TreeNode<T> root) {
        this.root = root;
    }


    @FunctionalInterface
    interface TraverseOperation<T> {
        ArrayList<T> combine(T node, ArrayList<T> left, ArrayList<T> right);
    }
}
