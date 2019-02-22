package ch.zhaw.its.lab.secretkey;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collections;

public class ComputeEntropy {

    public static void main(String[] args) {
        ComputeEntropy e = new ComputeEntropy();
        e.computeEntropyPerByte(args);
    }

    private void computeEntropyPerByte(String[] fileNames) {
        for (String name : fileNames) {
            try (InputStream is = new FileInputStream(name)) {
                System.out.println(name + ":" + computeEntropyPerByte(is));
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private double computeEntropyPerByte(InputStream is) throws IOException {
        int[] frequencies = Collections.nCopies(256, 0).stream().mapToInt(i -> i).toArray();
        int in;
        int length = 0;
        while ((in = is.read()) != -1) {
            frequencies[in]++;
            length++;
        }

        if (length == 0) {
            return 0;
        }

        final double len = (double) length;

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
