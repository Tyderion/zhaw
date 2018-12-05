package ch.isageek.ads.p2;

public class MyLinkedList {

    static class Node {
        Integer value;
        Node next;

        public Node(Integer value, Node next) {
            this.value = value;
            this.next = next;
        }

        public Node() {
            this.value = null;
            this.next = null;
        }

        public void setValue(Integer value) {
            this.value = value;
        }

        public Integer getValue() {
            return value;
        }

        public void setNext(Node next) {
            this.next = next;
        }

        public Node getNext() {
            return next;
        }
    }

    private Node head = null;

    public MyLinkedList() {
        this.head = new Node();
    }


    public int getFirst()  throws ListEmptyException {
        if (isEmpty()) {
            throw new ListEmptyException();
        }
        return getNode(0).getValue();
    }

    public int getLast() throws ListEmptyException {
        if (isEmpty()) {
            throw new ListEmptyException();
        }
        Node last = head;
        while (last.getNext() != null) {
            last = last.getNext();
        }
        return last.getValue();
    }

    public void addFirst(int value) {
        add(0, value);
    }

    public void addLast(int i) {
        Node last = head;
        while (last.getNext() != null) {
            last = last.getNext();
        }
        Node newNode = new Node(i, null);
        last.setNext(newNode);
    }

    public void add(int index, int value) {
        Node beforeInsert = head;
        if (index > 0)  { // insert to head
           beforeInsert = getNode(index-1);
        }
        Node newNode = new Node(value, beforeInsert.getNext());
        beforeInsert.setNext(newNode);
    }

    public int get(int index) {
        return getNode(index).getValue();
    }

    public int size() {
        int size = 0;
        Node curr = head;
        while (curr.getNext() != null) {
            curr = curr.getNext();
            size++;
        }
        return size;
    }

    public int removeFirst() throws ListEmptyException {
        if (isEmpty()) {
            throw new ListEmptyException();
        }
        Node removed = head.getNext();
        head.setNext(removed.getNext());
        return removed.getValue();
    }

    public int removeLast() throws ListEmptyException {
        if (isEmpty()) {
            throw new ListEmptyException();
        }
        Node secondLast = head;
        while (secondLast.getNext().getNext() != null) {
            secondLast = secondLast.getNext();
        }
        Node removed = secondLast.getNext();
        secondLast.setNext(null);
        return removed.getValue();
    }

    public int remove(int index) {
        if (index < 0) {
            throw new IndexOutOfBoundsException("Cannot remove elements with negative indices.");
        }
        Node beforeRemoved = getNode(index - 1);
        Node removed = beforeRemoved.getNext();
        beforeRemoved.setNext(removed.getNext());
        return removed.getValue();
    }

    public boolean isEmpty() {
        return this.head.getNext() == null;
    }

    private Node getNode(int index) {
        if (index < 0) {
            throw new IndexOutOfBoundsException("Cannot get elements with negative indices.");
        }
        int i = 0;
        Node ithNode = head;
        while (ithNode.getNext() != null && i <= index) {
            ithNode = ithNode.getNext();
            i++;
        }
        if (i < index) {
            throw new IndexOutOfBoundsException("Cannot get after end of list");
        }
        return ithNode;
    }
}
