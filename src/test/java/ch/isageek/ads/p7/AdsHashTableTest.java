package ch.isageek.ads.p7;

import org.junit.Test;

import java.util.Iterator;
import java.util.List;

import static java.util.Arrays.asList;
import static org.junit.Assert.*;

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
    public void testAddElementBigInitialSize() {

        HashTable<Integer> hashTable = new AdsHashTable<>(10);
        hashTable.add(15);

        assertTrue(hashTable.contains(15));
        hashTable.remove(15);
        assertFalse(hashTable.contains(15));
    }

    @Test
    public void testAddMultipleElementsWithSameHashCode() {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(2);

        CustomHashCode a = new CustomHashCode("a", 0);
        CustomHashCode b = new CustomHashCode("b", 0);

        hashTable.add(a);
        hashTable.add(b);

        assertTrue(hashTable.contains(a));
        assertTrue(hashTable.contains(b));


        final List<CustomHashCode> elements = asList(a, b);
        Iterator<CustomHashCode> it = hashTable.iterator();
        assertNotNull(it);
        int count = 0;
        while (it.hasNext()) {
            CustomHashCode next = it.next();
            assertEquals(elements.get(count), next);
            count++;
        }
        assertEquals(2, count);
    }

    @Test
    public void testAddLessElementsThanInitialSize() {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(20);
        hashTable.setLoadFactorForResize(1);

        CustomHashCode a = new CustomHashCode("a", 0);
        CustomHashCode b = new CustomHashCode("b", 5);
        CustomHashCode c = new CustomHashCode("c", 17);

        hashTable.add(a);
        hashTable.add(b);
        hashTable.add(c);

        assertTrue(hashTable.contains(a));
        assertTrue(hashTable.contains(b));
        assertTrue(hashTable.contains(c));


        final List<CustomHashCode> elements = asList(a, b, c);
        Iterator<CustomHashCode> it = hashTable.iterator();
        assertNotNull(it);
        int count = 0;
        while (it.hasNext()) {
            CustomHashCode next = it.next();
            assertEquals(elements.get(count), next);
            count++;
        }
        assertEquals(3, count);
    }

    @Test
    public void testAddMoreElementsThanInitialSize() {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(2);
        hashTable.setLoadFactorForResize(1);

        CustomHashCode a = new CustomHashCode("a", 0);
        CustomHashCode b = new CustomHashCode("b", 1);
        CustomHashCode c = new CustomHashCode("c", 2);

        hashTable.add(a);
        hashTable.add(b);
        hashTable.add(c);

        assertTrue(hashTable.contains(a));
        assertTrue(hashTable.contains(b));
        assertTrue(hashTable.contains(c));


        final List<CustomHashCode> elements = asList(a, b, c);
        Iterator<CustomHashCode> it = hashTable.iterator();
        assertNotNull(it);
        int count = 0;
        while (it.hasNext()) {
            CustomHashCode next = it.next();
            assertEquals(elements.get(count), next);
            count++;
        }
        assertEquals(3, count);
    }





    private static class CustomHashCode {
        String value;
        int hash;
        public CustomHashCode(String value, int hash) {
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
