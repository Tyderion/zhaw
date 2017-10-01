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


    public int getFirst() throws ListEmptyException {
        if (isEmpty()) {
            throw new ListEmptyException();
        }
        return this.head.getNext().getValue();
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

    public void addFirst(int i) {
        Node newNode = new Node(i, head.getNext());
        head.setNext(newNode);
    }

    public void addLast(int i) {
        Node last = head;
        while (last.getNext() != null) {
            last = last.getNext();
        }
        Node newNode = new Node(i, null);
        last.setNext(newNode);
    }

    public void add(int i, int value) {

    }

    public int get(int index) {
        return 0;
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
        return 0;
    }

    public boolean isEmpty() {
        return this.head.getNext() == null;
    }
}
