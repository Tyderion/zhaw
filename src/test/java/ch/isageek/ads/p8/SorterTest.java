package ch.isageek.ads.p8;

import static java.util.Arrays.asList;
import static java.util.Arrays.sort;
import static org.junit.Assert.*;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Collection;

@RunWith(Parameterized.class)
public class SorterTest {

	Sorter sorter;

	@Parameterized.Parameters(name = "{0}")
	public static Collection<Sorter> getSorters() {
		return asList(new InsertionSort(), new Quicksort(), new QuicksortMedian(), new QuicksortTurbo());
	}

	public SorterTest(Sorter sorter) {
		this.sorter = sorter;
	}

	@Test
	public void shouldNotDoAnythingForNull() {
		int[] parameter = null;
		sorter.sort(parameter);
		assertNull(parameter);
	}
	

	@Test
	public void shouldNotDoAnythingForSingleElement() {
		int[] parameter = {1};
		int[] expected = {1};
		sorter.sort(parameter);
		assertArrayEquals(expected, parameter);
	}
	
	@Test
	public void shouldKeepAlreadySortedArray() {
		int[] parameter = {-10, -3, 0, 0, 1, 1, 111, 39393939};
		int[] expected = {-10, -3, 0, 0, 1, 1, 111, 39393939};
		sorter.sort(parameter);
		assertArrayEquals(expected, parameter);
	}
	
	@Test
	public void shouldSortAscending() {
		int[] parameter = {4, -10, 2, 5, -10, 3, 202, -111, 0, 1};
		int[] expected = {-111, -10, -10, 0, 1, 2, 3, 4, 5, 202};
		sorter.sort(parameter);
		assertArrayEquals(expected, parameter);
	}
	
}
