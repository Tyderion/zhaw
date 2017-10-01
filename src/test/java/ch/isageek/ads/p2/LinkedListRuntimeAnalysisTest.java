package ch.isageek.ads.p2;

import org.junit.Test;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class LinkedListRuntimeAnalysisTest {
    private static final int ELEMENTS_TO_ADD = 50;
    private static final Random random = new Random();
    private static int[] SIZES = {100, 1000, 10000, 100000};

    @Test
    public void doComparison() throws InterruptedException {
        System.out.println(String.format("Inserting %d Elements at first/last/random positions in the lists.", ELEMENTS_TO_ADD));
        System.out.println("Size\tType\tMyList\tArrayList\tLinkedList");
        for (int size : SIZES) {
            ArrayList<Integer> alist = arrayListLength(size);
            LinkedList<Integer> llist = linkedListLength(size);
            MyLinkedList mllist = myLinkedListLength(size);
            for (Operations op : Operations.values()) {
                final Result result = new Result(size, op.name());
                result.durationArrayList = testExecutionTime(testList(alist, op.getIndex));
                result.durationLinkedList = testExecutionTime(testList(llist, op.getIndex));
                result.durationMyList = testExecutionTime(testList(mllist, op.getIndex));
                System.out.println(result.toString());
            }
        }
    }

    Execute testList(List<Integer> list, Function<Integer, Integer> index) {
        final int length = list.size();
        return () -> {
            for (int i = 0; i < ELEMENTS_TO_ADD; i++) {
                list.add(index.apply(length), i);
            }
        };
    }

    Execute testList(MyLinkedList list, Function<Integer, Integer> index) {
        final int length = list.size();
        return () -> {
            for (int i = 0; i < ELEMENTS_TO_ADD; i++) {
                list.add(index.apply(length), i);
            }
        };
    }

    private ArrayList<Integer> arrayListLength(int length) {
        return IntStream.range(0, length).boxed().collect(Collectors.toCollection(() -> new ArrayList<>(length)));
    }

    private LinkedList<Integer> linkedListLength(int length) {
        return IntStream.range(0, length).boxed().collect(Collectors.toCollection(LinkedList::new));
    }

    private MyLinkedList myLinkedListLength(int length) {
        MyLinkedList list = new MyLinkedList();
        IntStream.range(0, length).forEach(list::addLast);
        return list;
    }

    private long testExecutionTime(Execute operation) {
        long startTime = System.nanoTime();
        operation.execute();
        long endTime = System.nanoTime();
        return endTime - startTime;
    }

    enum Operations {
        START((length) -> 0),
        LAST((length) -> length),
        RANDOM(random::nextInt);

        public Function<Integer, Integer> getIndex;

        Operations(Function<Integer, Integer> getIndex) {
            this.getIndex = getIndex;
        }
    }

    @FunctionalInterface
    private static interface Execute {
        void execute();
    }

    private static class Result {
        long durationLinkedList;
        long durationArrayList;
        long durationMyList;
        int itemCount;
        String name;
        Result(int count, String name) {
            this.itemCount = count;
            this.name = name;
        }

        @Override
        public String toString() {
            return String.format("%d\t%s\t%d\t%d\t%d", itemCount, name, durationMyList, durationArrayList, durationLinkedList);
        }
    }

}
