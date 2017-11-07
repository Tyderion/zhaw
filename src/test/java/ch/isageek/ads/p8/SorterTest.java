package ch.isageek.ads.p8;

import static java.util.Arrays.asList;
import static java.util.Arrays.sort;
import static org.junit.Assert.*;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Arrays;
import java.util.Collection;
import java.util.Random;

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
	public void shouldKeepAlreadySortedArray2() {
		int[] parameter = {-111, -10, -10};
		int[] expected = {-111, -10, -10};
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

	@Test
	public void shouldSortAscending2() {
		int[] parameter = {4, -10, 2, 5, -9, 3, 202, -111, 0, 1};
		int[] expected = {-111, -10, -9, 0, 1, 2, 3, 4, 5, 202};
		sorter.sort(parameter);
		assertArrayEquals(expected, parameter);
	}
	@Test
	public void shouldSortAscending3() {
		int[] parameter = {204, 79, 249, 95, 183, 49, 128, 118, 13, 202};
		int[] expected = {13, 49, 79, 95, 118, 128, 183, 202, 204, 249};
		sorter.sort(parameter);
		assertArrayEquals(expected, parameter);
	}

	@Test
	public void shouldSortAscending4() {
		int[] parameter = {13, 79, 118, 95, 128, 49};
		int[] expected = {13, 49, 79, 95, 118, 128};
		sorter.sort(parameter);
		assertArrayEquals(expected, parameter);
	}

	@Test
	public void shouldSortRandomArray() {
		int size = sorter instanceof InsertionSort ? 100000 : 100000000;
		int[] parameter = randomArray(size);

		int[] expeced = new int[size];
		System.arraycopy(parameter, 0, expeced, 0, size);
		Arrays.sort(expeced); // Sort by a known working implementation

		sorter.sort(parameter);

		assertArrayEquals(expeced, parameter);
	}


	public int[] randomArray(int n)
	{
		int[] list = new int[n];
		Random random = new Random();

		for (int i = 0; i < n; i++)
		{
			list[i] = random.nextInt(255);
		}
		return list;
	}
}
