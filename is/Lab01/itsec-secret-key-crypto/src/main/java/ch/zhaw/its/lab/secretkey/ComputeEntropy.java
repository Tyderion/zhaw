package ch.zhaw.its.lab.secretkey;

import java.util.Arrays;
import java.util.Collections;

public class ComputeEntropy {

    public static void main(String[] args) {
        ComputeEntropy e = new ComputeEntropy();
        e.computeEntropyPerByte(args);
    }

    private void computeEntropyPerByte(String[] fileNames) {
        FileHelper.doForFiles(fileNames, bytes -> {
            System.out.println(bytes.getKey() + ":" + computeEntropyPerByte(bytes.getValue()));
        });
    }

    public double computeEntropyPerByte(final byte[] in) {
        int[] frequencies = Collections.nCopies(256, 0).stream().mapToInt(i -> i).toArray();
        for (byte b : in) {
            frequencies[b+128]++;
        }

        if (in.length == 0) {
            return 0;
        }

        final double len = (double) in.length;
        return -Arrays.stream(frequencies)
                .mapToDouble(frequency -> frequency / len)
                .filter(relativeFreq -> relativeFreq > 0)
                .map(relativeFreq -> relativeFreq * log2(relativeFreq))
                .sum();
    }

    private double log2(double x) {
        return Math.log(x) / Math.log(2d);
    }
}
