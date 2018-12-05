package ch.isageek.ads.p3;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;

public class BinaryTree<T extends Comparable<T>> {

    protected TreeNode<T> root;

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
        final Queue<TreeNode<T>> queue = new LinkedList<>();
        queue.add(root);
        final ArrayList<T> traversal = new ArrayList<>();
        while (!queue.isEmpty()) {
            final TreeNode<T> node = queue.poll();
            if (node != null) {
                traversal.add(node.getElement());
                queue.add(node.getLeft());
                queue.add(node.getRight());
            }
        }

        return traversal;
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

    private ArrayList<T> traverse(final TreeNode<T> root, final TraverseOrder<T> traverseOrder) {
        if (root == null) {
            return new ArrayList<>();
        }
        final ArrayList<T> leftTraversal = traverse(root.getLeft(), traverseOrder);
        final ArrayList<T> rightTraversal = traverse(root.getRight(), traverseOrder);

        return traverseOrder.combine(root.getElement(), leftTraversal, rightTraversal);
    }

    public TreeNode<T> getRoot() {
        return root;
    }

    public void setRoot(final TreeNode<T> root) {
        this.root = root;
    }


    @FunctionalInterface
    interface TraverseOrder<T> {
        ArrayList<T> combine(final T node, final ArrayList<T> left, final ArrayList<T> right);
    }
}
