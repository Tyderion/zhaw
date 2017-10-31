package ch.isageek.ads.p7;

import org.junit.Test;

import java.lang.reflect.Field;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static java.util.Arrays.asList;
import static org.junit.Assert.*;

public class AdsHashTableTest {

    @Test
    public void testIsEmpty() {
        HashTable<Integer> hashTable = new AdsHashTable<>(1);
        assertTrue(hashTable.isEmpty());
        hashTable.add(1);
        assertFalse(hashTable.isEmpty());
        hashTable.remove(1);
        assertTrue(hashTable.isEmpty());
    }

    @Test
    public void testAddContainsElement() {
        HashTable<Integer> hashTable = new AdsHashTable<>(1);
        hashTable.add(1);

        assertTrue(hashTable.contains(1));
        assertFalse(hashTable.contains(2));
        assertEquals(1, hashTable.size());
    }

    @Test
    public void testRemoveContainsElement() {

        HashTable<Integer> hashTable = new AdsHashTable<>(1);
        hashTable.add(1);

        assertTrue(hashTable.contains(1));
        assertEquals(1, hashTable.size());
        hashTable.remove(1);
        assertFalse(hashTable.contains(1));
    }

    @Test
    public void testAddElementBigInitialSize() {

        HashTable<Integer> hashTable = new AdsHashTable<>(10);
        hashTable.add(15);

        assertTrue(hashTable.contains(15));
        assertEquals(1, hashTable.size());
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
        assertEquals(2, hashTable.size());


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
    public void testRemoveElementWithSameHashCode() {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(10);
        final List<CustomHashCode> elements = generateObjects(3, 0, 0, 0);

        hashTable.addAll(elements);
        elements.forEach(ele -> assertTrue(hashTable.contains(ele)));
        hashTable.remove(elements.get(1));

        assertTrue(hashTable.contains(elements.get(0)));
        assertTrue(hashTable.contains(elements.get(2)));
        assertFalse(hashTable.contains(elements.get(1)));
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
        assertEquals(3, hashTable.size());


        final List<CustomHashCode> elements = asList(a, b, c);
        Iterator<CustomHashCode> it = hashTable.iterator();
        assertNotNull(it);
        int count = 0;
        while (it.hasNext()) {
            CustomHashCode next = it.next();
            if (count == 0 || count == 5 || count == 17) {
                assertEquals(elements.get(count == 0 ? 0 : count == 5 ? 1 : 2), next);
            } else {
                assertNull(next);
            }
            count++;
        }
        assertEquals(20, count);
    }

    @Test
    public void testGrowWithLoadfactorOne() {
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
        assertEquals(3, hashTable.size());


        final List<CustomHashCode> elements = asList(a, b, c);
        Iterator<CustomHashCode> it = hashTable.iterator();
        assertNotNull(it);
        int count = 0;
        while (it.hasNext()) {
            CustomHashCode next = it.next();
            if (count <= 2) {
                assertEquals(elements.get(count), next);
            } else {
                assertNull(next);
            }
            count++;
        }
    }

    @Test
    public void testAddAll() {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(2);
        List<CustomHashCode> objects = generateObjects(2);
        hashTable.addAll(objects);

        assertEquals(2, hashTable.size());
    }

    @Test
    public void testGrowWithLoadfactorHalf() throws Exception {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(10);
        hashTable.setLoadFactorForResize(0.5f);
        final List<CustomHashCode> elements = generateObjects(6);
        hashTable.addAll(elements.subList(0, 5));

        assertEquals(5, hashTable.size());

        hashTable.add(elements.get(5));

        assertEquals(6, hashTable.size());

        Field table = AdsHashTable.class.getDeclaredField("table");
        table.setAccessible(true);
        Object[] actualTable = (Object[]) table.get(hashTable);
        assertNotEquals(10, actualTable.length);
        assertTrue(actualTable.length > 10);
    }

    @Test
    public void testLinearProbing() throws Exception {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(5, AdsHashTable.ProbingMode.LINEAR);
        hashTable.setLoadFactorForResize(1);
        final List<CustomHashCode> elements = generateObjects(3, 0, 0, 0);
        final List<CustomHashCode> resultingList = asList(
                elements.get(0),
                elements.get(1),
                elements.get(2),
                null,
                null
        );
        hashTable.addAll(elements);

        assertReflectionEquals(resultingList, hashTable.stream().collect(Collectors.toList()));
    }

    @Test
    public void testLinearProbingMiddle() throws Exception {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(5, AdsHashTable.ProbingMode.LINEAR);
        hashTable.setLoadFactorForResize(1);
        final List<CustomHashCode> elements = generateObjects(3, 1, 1, 1);
        final List<CustomHashCode> resultingList = asList(
                null,
                elements.get(0),
                elements.get(1),
                elements.get(2),
                null
        );
        hashTable.addAll(elements);

        assertReflectionEquals(resultingList, hashTable.stream().collect(Collectors.toList()));
    }

    /**
     *  Quadratic for 2 collisions is: 3, 7, see {@link QuadraticProbe}
     *  */
    @Test
    public void testQuadraticProbing() throws Exception {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(10, AdsHashTable.ProbingMode.QUADRATIC);
        final List<CustomHashCode> elements = generateObjects(3, 0, 0, 0);
        final List<CustomHashCode> resultingList = asList(
                elements.get(0), // no collision
                null,
                null,
                elements.get(1), // first collision + 3
                null,
                null,
                null,
                elements.get(2), // second collision + 7
                null,
                null
        );

        hashTable.addAll(elements);

        assertReflectionEquals(resultingList, hashTable.stream().collect(Collectors.toList()));
    }


    /**
     *  Quadratic for 2 collisions is: 3, 7, see {@link QuadraticProbe}
     *  */
    @Test
    public void testQuadraticProbingMiddle() throws Exception {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(12, AdsHashTable.ProbingMode.QUADRATIC);
        final List<CustomHashCode> elements = generateObjects(3, 2, 2, 2);
        final List<CustomHashCode> resultingList = asList(
                null,
                null,
                elements.get(0), // no collision
                null,
                null,
                elements.get(1), // first collision + 3
                null,
                null,
                null,
                elements.get(2), // second collision + 7
                null,
                null
        );

        hashTable.addAll(elements);

        assertReflectionEquals(resultingList, hashTable.stream().collect(Collectors.toList()));
    }

    private void assertReflectionEquals(List<CustomHashCode> elements, List<CustomHashCode> collect) {
    }


    private List<CustomHashCode> generateObjects(int amount, int... hashcodes) {
        int defaultHashcode = hashcodes.length > 0 ? hashcodes[0] : amount;
        return IntStream.range(0, amount)
                .mapToObj(i -> new CustomHashCode(Character.toString((char) (i + 65)), hashcodes.length > i ? hashcodes[i] : defaultHashcode))
                .collect(Collectors.toList());
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
            return obj instanceof CustomHashCode && ((CustomHashCode) obj).value == value;
        }
    }
}
