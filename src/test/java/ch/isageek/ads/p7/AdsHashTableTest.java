package ch.isageek.ads.p7;

import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class AdsHashTableTest {

    @Test
    public void testAddContainsElement() {
        HashTable<Integer> hashTable = new AdsHashTable<>(1);
        hashTable.add(1);

        assertTrue(hashTable.contains(1));
        assertFalse(hashTable.contains(2));
    }

    @Test
    public void testRemoveContainsElement() {

        HashTable<Integer> hashTable = new AdsHashTable<>(1);
        hashTable.add(1);

        assertTrue(hashTable.contains(1));
        hashTable.remove(1);
        assertFalse(hashTable.contains(1));
    }

    @Test
    public void testAddMultipleElementsWithSameHashCode() {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(2);

        CustomHashCode a = new CustomHashCode(1, 1);
        CustomHashCode b = new CustomHashCode(2, 1);

        hashTable.add(a);
        hashTable.add(b);

        assertTrue(hashTable.contains(a));
        assertTrue(hashTable.contains(b));
    }

    private static class CustomHashCode {
        int value;
        int hash;
        public CustomHashCode(int value, int hash) {
            this.value = value;
            this.hash = hash;
        }

        @Override
        public int hashCode() {
            return hash;
        }

        @Override
        public boolean equals(Object obj) {
            return obj instanceof CustomHashCode && ((CustomHashCode)obj).value == value;
        }
    }
}
