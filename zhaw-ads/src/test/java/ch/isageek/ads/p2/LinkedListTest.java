package ch.isageek.ads.p2;

import static org.junit.Assert.*;

import org.junit.Test;


public class LinkedListTest {
	
	@Test
	public void shouldReturnElementAtFirstPosition() throws ListEmptyException{
		assertEquals(11, createListWithManyElements().getFirst());
	}
	
	@Test
	public void shouldReturnElementAtLastPosition() throws ListEmptyException{
		assertEquals(44, createListWithManyElements().getLast());
	}
	
	@Test
	public void shouldAddFirstCorrectly() throws ListEmptyException{
		MyLinkedList listFrom1To9 = createSortedFrom1To9();
		listFrom1To9.addFirst(0);
		
		assertEquals("Chech if the first Element of the list is zero",
				0, listFrom1To9.getFirst());
	}
	
	@Test
	public void shoudlAddLastCorrectly() throws ListEmptyException{
		MyLinkedList listFrom1To9 = createSortedFrom1To9();
		listFrom1To9.addLast(10);
		
		assertEquals("Chech if the last Element of the list is 10 by getLast",
				10, listFrom1To9.getLast());
	}

	@Test
	public void shouldContainOneElement(){
		MyLinkedList listWithSingleElement = new MyLinkedList();
		listWithSingleElement.addFirst(42);

		assertEquals("List contains only one Element", 1, listWithSingleElement.size());
	}
	
	@Test
	public void shouldContainFourEelement(){
		assertEquals("List contains four Element", 4, createListWithManyElements().size());
	}
	
	@Test
	public void shoudInsertAtCorrectIndex() throws ListEmptyException{
		MyLinkedList list = new MyLinkedList();
		list.add(0,3);
		list.add(0,1);
		list.add(1,2);
		
		assertEquals(1, list.getFirst());
		assertEquals(3, list.getLast());
		assertEquals(2, list.get(1));
	}
	
	@Test
	public void shouldGetCorrectValueAtIndex(){
		MyLinkedList listFrom1To9 = createSortedFrom1To9();
		for (int index = 1 ; index > 10 ; index ++)
			assertEquals(index, listFrom1To9.get(index));
	}
	
	@Test 
	public void indexAtAndGetFirstOrLastShouldBeTheSame() throws ListEmptyException{
		MyLinkedList listFrom1To9 = createSortedFrom1To9();
		
		assertEquals(listFrom1To9.getFirst(), listFrom1To9.get(0));
		assertEquals(listFrom1To9.getLast(), listFrom1To9.get(listFrom1To9.size()-1));
	}
	
	@Test
	public void removeFirstShouldBeTheSameAsGetFirstAndRemoveIt() throws ListEmptyException{
		MyLinkedList listFrom1To9 = createSortedFrom1To9();
		int firstElement = listFrom1To9.getFirst();
		
		assertEquals(listFrom1To9.getFirst(), listFrom1To9.removeFirst());
		assertNotEquals(firstElement, listFrom1To9.getFirst());
		assertEquals(8, listFrom1To9.size());
	}
	
	@Test
	public void removeLastShouldBeTheSameAsGetLastAndRemoveIt() throws ListEmptyException{
		MyLinkedList listFrom1To9 = createSortedFrom1To9();
		int lastElement = listFrom1To9.getFirst();
		
		assertEquals(listFrom1To9.getLast(), listFrom1To9.removeLast());
		assertNotEquals(lastElement, listFrom1To9.getLast());
		assertEquals(8, listFrom1To9.size());
	}
	
	@Test
	public void removeAtShouldBeTheSameAsGetAtAndRemoveIt() throws ListEmptyException{
		MyLinkedList listFrom1To9 = createSortedFrom1To9();
		int elementAt4 = listFrom1To9.get(4);
		
		assertEquals(listFrom1To9.get(4), listFrom1To9.remove(4));
		assertNotEquals(elementAt4, listFrom1To9.get(4));
		assertEquals(8, listFrom1To9.size());
	}
	
	
	@Test
	public void shouldReturnTrueOnEmptyList(){
		assertTrue((new MyLinkedList()).isEmpty());
	}
	
	@Test
	public void shouldReturnFalseOnNonemptyList(){
		assertFalse(createListWithManyElements().isEmpty());
	}
	
	@Test(expected = ListEmptyException.class)
	public void shouldFailToGetFirstElementFromEmptyList() throws ListEmptyException{
		MyLinkedList listWithZeroElements;
		listWithZeroElements = new MyLinkedList();

		listWithZeroElements.getFirst();
	}
	
	@Test(expected = ListEmptyException.class)
	public void shouldFailToGetLastElementFromEmptyList() throws ListEmptyException{
		MyLinkedList listWithZeroElements;
		listWithZeroElements = new MyLinkedList();

		listWithZeroElements.getLast();
	}
	
	@Test(expected = IndexOutOfBoundsException.class)
	public void shouldFailToGetTooLargeIndex() throws ListEmptyException{

		MyLinkedList listWithManyElements = createListWithManyElements();
		
		listWithManyElements.get(99);
	}

	@Test(expected = IndexOutOfBoundsException.class)
	public void shouldFailToGetNegativeIndex() throws IndexOutOfBoundsException{

		MyLinkedList listWithManyElements = createListWithManyElements();
		
		listWithManyElements.get(-1);
	}

	@Test(expected = ListEmptyException.class)
	public void shouldFailToRemoveFirstElementFromEmptyList() throws ListEmptyException{
		MyLinkedList listWithZeroElements = new MyLinkedList();

		listWithZeroElements.removeFirst();
	}
	
	@Test(expected = ListEmptyException.class)
	public void shouldFailToRemoveLastElementFromEmptyList() throws ListEmptyException{
		MyLinkedList listWithZeroElements = new MyLinkedList();

		listWithZeroElements.removeLast();
	}
	
	
	@Test(expected = IndexOutOfBoundsException.class)
	public void shouldFailToRemoveTooLargeIndex() throws ListEmptyException{

		MyLinkedList listWithManyElements = createListWithManyElements();
		
		listWithManyElements.remove(99);
	}
	
	@Test(expected = IndexOutOfBoundsException.class)
	public void shouldFailToRemoveNegativeIndex() throws IndexOutOfBoundsException{

		MyLinkedList listWithManyElements = createListWithManyElements();
		
		listWithManyElements.remove(-1);
	}

	@Test(expected = IndexOutOfBoundsException.class)
	public void shouldThrowExeptionIfAddingWithTooLargeIndex(){
		MyLinkedList listWithSingleElement = new MyLinkedList();
		
		listWithSingleElement.addFirst(42);

		listWithSingleElement.add(99, 2);
	}
	
	@Test(expected = IndexOutOfBoundsException.class)
	public void shouldFailIfAddingWithNegativeIndex(){
		MyLinkedList listWithSingleElement = new MyLinkedList();
		
		listWithSingleElement.addFirst(42);

		listWithSingleElement.add(99, -1);
	}

	
	private MyLinkedList createListWithManyElements() {
		MyLinkedList listWithManyElements = new MyLinkedList();
		
		listWithManyElements.addFirst(22);
		listWithManyElements.addFirst(11);
		listWithManyElements.addLast(44);
		listWithManyElements.add(2,33);
	
		return listWithManyElements;
	}
	
	private MyLinkedList createSortedFrom1To9(){
		MyLinkedList list = new MyLinkedList();
		for (int i = 1; i < 10; i++)
			list.addLast(i);
		return list;
	}
}
