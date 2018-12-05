package ch.isageek.ads.p1;

public class ThreeSum {
    public static int countThreeSums(int[] input) {
        int count = 0;
        for (int i = 0; i < input.length-2; i++) {
            for (int j = i+1; j < input.length-1; j++) {
                for (int k = j+1; k < input.length; k++) {
                    if (input[i] + input[j] + input[k] == 0) {
                        count++;
                    }
                }
            }
        }
        return count;
    }
}

