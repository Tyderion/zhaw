package ch.isageek.ads.p1;

import java.util.Random;

public class Util {
	
	/**
	 * 
	 * Generates an array with random integers.
	 * 
	 * @param length length of the array
	 * @param maximumValue each element in the array will be within [-maxValue, maxValue]  
	 * @return
	 */
	public static int[] generateArrayWithIntegers(int length, int maximumValue) {
		int[] randomArray = new int[length];
		Random rand = new Random();
		for (int i = 0; i < randomArray.length; i++) {
			randomArray[i] = rand.nextInt(2 * maximumValue) - maximumValue;
		}
		return randomArray;
	}
	
}
