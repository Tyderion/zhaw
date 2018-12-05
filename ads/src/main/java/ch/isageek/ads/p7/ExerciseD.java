package ch.isageek.ads.p7;

import java.util.Arrays;
import java.util.Iterator;
import java.util.stream.Collectors;

public class ExerciseD {
    private static Integer[] NUMBERS = {12, 44, 13, 88, 23, 94, 11, 39, 20, 16, 5};

    public static void main(String[] args) {
        System.out.println("Aufgabe d)");
        System.out.println("Hashtable mit linearem sondieren, initiale Grösse 1, loadfactor 1");

        AdsHashTable<Exersice4Element> exerciseD = new AdsHashTable<>(11, AdsHashTable.ProbingMode.LINEAR);
        exerciseD.setLoadFactorForResize(1.0f);

        exerciseD.addAll(Arrays.stream(NUMBERS).map(Exersice4Element::new).collect(Collectors.toList()));

        System.out.println("Inhalt nach einfügen von allen 11 Elementen");
        printTable(exerciseD);
        System.out.println("Die Werte werden in die gleichen Buckets gespeichert wie von Hand bei der Fingerübung.");
    }

    private static void printTable(HashTable<Exersice4Element> hashTable) {
        int index = 0;
        Iterator<Exersice4Element> it = hashTable.iterator();

        while (it.hasNext()) {
            System.out.println(String.format("%d\t%s", index, it.next()));
            index++;
        }
    }

    private static class Exersice4Element {
        private final int value;

        private Exersice4Element(int value) {
            this.value = value;
        }

        @Override
        public int hashCode() {
            return (2 * value + 5) % 11;
        }

        @Override
        public boolean equals(Object obj) {
            return obj instanceof  Exersice4Element && ((Exersice4Element) obj).value == this.value;
        }

        @Override
        public String toString() {
            return String.valueOf(value);
        }
    }
}
