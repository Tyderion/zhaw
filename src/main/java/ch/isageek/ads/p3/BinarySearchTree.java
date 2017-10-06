package ch.isageek.ads.p3;

public class BinarySearchTree<T extends Comparable<T>> extends BinaryTree<T> {

    public BinarySearchTree() {
        super();
    }

    public BinarySearchTree(T value) {
        super(value);
    }


    public void add(T element) {
        if (root == null) {
            root = new TreeNode<>(element);
        } else {
            addRecursively(element, root);
        }
    }

    /**
     * Finds an element in the binary tree
     * @param element The element to find
     * @return the found element or null if not in the tree
     */
    public T find(T element) {
        return findRecursively(element, root);
    }

    private void addRecursively(T element, TreeNode<T> node) {
        if (element.compareTo(node.getElement()) > 0) {
            if (node.getRight() == null) {
                node.setRight(new TreeNode<>(element));
            } else {
                addRecursively(element, node.getRight());
            }
        } else if (element.compareTo(node.getElement()) < 0) {
            if (node.getLeft() == null) {
                node.setLeft(new TreeNode<>(element));
            } else {
                addRecursively(element, node.getLeft());
            }
        }
        // The element to insert is the same as in the node, doing nothing
    }

    private T findRecursively(T element, TreeNode<T> node) {
        if (element.equals(node.getElement())) {
            return node.getElement();
        }
        if (element.compareTo(node.getElement()) > 0) {
            return findRecursively(element, node.getRight());
        }

        if (element.compareTo(node.getElement()) < 0) {
            return findRecursively(element, node.getLeft());
        }
        return null;
    }
}
