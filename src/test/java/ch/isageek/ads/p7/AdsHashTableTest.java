package ch.isageek.ads.p7;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static java.util.Arrays.asList;
import static org.junit.Assert.*;
import static org.unitils.reflectionassert.ReflectionAssert.assertReflectionEquals;

@RunWith(Parameterized.class)
public class AdsHashTableTest {

    private AdsHashTable.ProbingMode probingMode;

    public AdsHashTableTest(AdsHashTable.ProbingMode probingMode) {
        this.probingMode = probingMode;
    }

    @Parameterized.Parameters(name = "{0}")
    public static Collection<AdsHashTable.ProbingMode> getClasses() {
        return asList(AdsHashTable.ProbingMode.LINEAR, AdsHashTable.ProbingMode.QUADRATIC);
    }

    @Test
    public void testIsEmpty() {
        HashTable<Integer> hashTable = new AdsHashTable<>(1, probingMode);
        assertTrue(hashTable.isEmpty());
        hashTable.add(1);
        assertFalse(hashTable.isEmpty());
        hashTable.remove(1);
        assertTrue(hashTable.isEmpty());
    }

    @Test
    public void testAddContainsElement() throws Exception{
        HashTable<Integer> hashTable = new AdsHashTable<>(1, probingMode);
        hashTable.add(0);

        assertTrue(hashTable.contains(0));
        assertFalse(hashTable.contains(1));
        assertEquals(1, hashTable.size());

        Field table = AdsHashTable.class.getDeclaredField("table");
        table.setAccessible(true);
        Object[] actualTable = (Object[]) table.get(hashTable);
        assertNotNull(actualTable[0]);
        if (probingMode == AdsHashTable.ProbingMode.QUADRATIC) {
            assertNull(actualTable[1]);
            assertNull(actualTable[2]);
        }
    }

    @Test
    public void testRemoveContainsElement() {

        HashTable<Integer> hashTable = new AdsHashTable<>(1, probingMode);
        hashTable.add(1);

        assertTrue(hashTable.contains(1));
        assertEquals(1, hashTable.size());
        hashTable.remove(1);
        assertFalse(hashTable.contains(1));
    }

    @Test
    public void testAddElementBigInitialSize() {

        HashTable<Integer> hashTable = new AdsHashTable<>(10, probingMode);
        hashTable.add(15);

        assertTrue(hashTable.contains(15));
        assertEquals(1, hashTable.size());
        hashTable.remove(15);
        assertFalse(hashTable.contains(15));
    }

    @Test
    public void testAddMultipleElementsWithSameHashCode() {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(2, probingMode);

        CustomHashCode a = new CustomHashCode("a", 0);
        CustomHashCode b = new CustomHashCode("b", 0);

        hashTable.add(a);
        hashTable.add(b);

        assertTrue(hashTable.contains(a));
        assertTrue(hashTable.contains(b));
        assertEquals(2, hashTable.size());

        assertReflectionEquals(asList(a, b), hashTable.stream().filter(Objects::nonNull).collect(Collectors.toList()));
    }

    @Test
    public void testRemoveElementWithSameHashCode() {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(10,probingMode);
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
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(20, probingMode);
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

        assertReflectionEquals(asList(a, b, c), hashTable.stream().filter(Objects::nonNull).collect(Collectors.toList()));
    }

    @Test
    public void testGrowWithLoadfactorOne() {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(2, probingMode);
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
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(2, probingMode);
        List<CustomHashCode> objects = generateObjects(2);
        hashTable.addAll(objects);

        assertEquals(2, hashTable.size());
    }

    @Test
    public void testGrowWithLoadfactorHalf() throws Exception {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(10, probingMode);
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

    @Test
    public void testQuadraticProbing() throws Exception {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(10, AdsHashTable.ProbingMode.QUADRATIC);
        final List<CustomHashCode> elements = generateObjects(5, 0, 0, 0, 0,0);
        final List<CustomHashCode> resultingList = asList(
                elements.get(0), // no collision
                elements.get(1), // +1
                null,
                null,
                elements.get(3), // + 4
                null,
                null,
                elements.get(4), // -4
                null,
                null,
                elements.get(2) // - 1
        );

        hashTable.addAll(elements);

        assertReflectionEquals(resultingList, hashTable.stream().collect(Collectors.toList()));
    }

    @Test
    public void testQuadraticProbingMiddle() throws Exception {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(10, AdsHashTable.ProbingMode.QUADRATIC);
        final List<CustomHashCode> elements = generateObjects(5, 2, 2, 2, 2, 2);
        final List<CustomHashCode> resultingList = asList(
                null,
                elements.get(2), // - 1
                elements.get(0), // no collision
                elements.get(1), // +1
                null,
                null,
                elements.get(3), // + 4
                null,
                null,
                elements.get(4), // -4
                null
        );

        hashTable.addAll(elements);

        assertReflectionEquals(resultingList, hashTable.stream().collect(Collectors.toList()));
    }

    @Test
    public void testAddManyElements() throws Exception {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(100, probingMode);
        final List<CustomHashCode> elements = generateObjects(5000);

        hashTable.addAll(elements);

        assertEquals(5000, hashTable.size());
    }

    @Test
    public void testAddManyElementsWithIdenticalHashCode() throws Exception {
        HashTable<CustomHashCode> hashTable = new AdsHashTable<>(1, probingMode);
        final List<CustomHashCode> elements = generateObjects(200 ,IntStream.range(0, 200).map(i -> 1).toArray());

        hashTable.addAll(elements);

        assertEquals(200, hashTable.size());
    }

    private List<CustomHashCode> generateObjects(int amount, int... hashcodes) {
        int defaultHashcode = hashcodes.length > 0 ? hashcodes[0] : amount;
        return IntStream.range(0, amount)
                .mapToObj(i -> new CustomHashCode(
//                        UUID.randomUUID().toString(),
                        String.valueOf(i),
                        hashcodes.length > i ? hashcodes[i] : i))
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

        @Override
        public String toString() {
            return String.format("(%s: %s)", hash, value);
        }
    }
}
