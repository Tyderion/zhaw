package ch.isageek.ads.p1;

import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;

public class ArraySearchTest {
	
	
	
	// Sequential search Tests
	
	@Test
	public void shouldFindAllElementsInArrayWithSequencialSearch(){

		int[] positiveNumArrayWith1000Val = createPositiveNumArray(1000);
				
		for (int i : positiveNumArrayWith1000Val) {
			assertEquals("Key, position and value should have been: "+i,i, ArraySearch.sequentialSearch(positiveNumArrayWith1000Val, i));
		}
	}
	
	@Test
	public void shouldNotFindKeyIn10ElementArrayWithSequentialSearch(){
		
		int[] positiveNumArrayWith1000Val = createPositiveNumArray(1000);
		int keyNotIn = 420000000;
		
		assertEquals(ArraySearch.KEY_NOT_FOUND, ArraySearch.sequentialSearch(positiveNumArrayWith1000Val, keyNotIn));
	}
	
	@Test
	public void shouldFindNegativeElementInArrayWithSequencialSearch(){
		
		int negativeNumKey = -4;
		int negativeNumPos = 5;
		int[] negativeNumArray = new int[] {-15,-11,-8,-6,-6,-4,-3};
		
		assertEquals(negativeNumPos, ArraySearch.sequentialSearch(negativeNumArray, negativeNumKey));
	}
	
	@Test
	public void shouldNotFindKeyInEmtpyArrayWithSequencialSearch(){
		int[] emptyArray = {};
		int anyKey = 27;
		
		assertEquals(ArraySearch.KEY_NOT_FOUND, ArraySearch.sequentialSearch(emptyArray, anyKey));
	}
	
	
	
	
	// Binary search Tests
	
	@Test
	public void shouldFindAllElementsInArrayWithBinarySearch(){
		int[] positiveNumArrayWith1000Val = createPositiveNumArray(1000);
		
		for (int i : positiveNumArrayWith1000Val) {
			assertEquals(i, ArraySearch.binarySearch(positiveNumArrayWith1000Val, i));
		}
	}
	
	@Test
	public void shouldNotFindKeyIn10ElementArrayWithBinarySearch(){
		int[] positiveNumArrayWith1000Val = createPositiveNumArray(1000);
		int keyNotIn = 4200000;
		
		assertEquals(ArraySearch.KEY_NOT_FOUND, ArraySearch.binarySearch(positiveNumArrayWith1000Val, keyNotIn));
	}
	
	@Test
	public void shouldFindNegativeElementInArrayWithBinarySearch(){
		int negativeNumKey = -4;
		int negativeNumPos = 5;
		int[] negativeNumArray = new int[] {-15,-11,-8,-6,-6,-4,-3};
		
		assertEquals(negativeNumPos, ArraySearch.binarySearch(negativeNumArray, negativeNumKey));
	}
	
	@Test
	public void shouldNotFindKeyInEmtpyArrayWithBinarySearch(){
		
		int[] emptyArray = {};
		int anyKey = 27;
		
		assertEquals(ArraySearch.KEY_NOT_FOUND, ArraySearch.binarySearch(emptyArray, anyKey));
	}

	private int[] createPositiveNumArray(int length) {
		int[] result = new int[length];
		for (int i = 0; i < length; i++) {
			result[i] = i;
		}
		return result;
	}
}
