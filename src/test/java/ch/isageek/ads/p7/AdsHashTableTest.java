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
}
