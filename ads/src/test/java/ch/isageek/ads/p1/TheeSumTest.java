package ch.isageek.ads.p1;

import static org.junit.Assert.*;
import org.junit.Test;

public class TheeSumTest {

	@Test
	public void shouldFindNoThreeSums() {
		int[] inputArray = {-20, -10, 10, 13};
		assertEquals(ThreeSum.countThreeSums(inputArray), 0);
	}

	@Test
	public void shouldFindThreeSums() {
		int[] inputArray = 
			{ 16, -6, 9, 18, -14, -11, 13, 17, -18, 12, -7, 14, -13, 0, 4, -19, 0, -8, 7, -14, -9, 12, -16, 3, -14, 
					-10, -15, 9, 5, 14, -11, -13, 19, 17, -5, 17, -11, -19, 3, -13, 5, 4, -11, -12, 0, -14, 1, 14, 
					-6, -20, -16, 19, -20, -1, 10, -19, 8, -6, 1, 3, -11, -6, 11, 8, 6, -16, -19, -7, -12, -16, 18, 
					11, 4, 17, 3, -17, 12, 4, 8, -4, -2, 19, -12, 15, 15, -9, -1, 8, 9, -19, -2, 4, -12, -17, -19, 
					-5, 12, -5, 12, -18	};
		assertEquals(ThreeSum.countThreeSums(inputArray), 2774);
	}

	@Test
	public void shouldTakeAroundTenSeconds() {
		ThreeSum.countThreeSums(Util.generateArrayWithIntegers(3500, 400));
	}

}
