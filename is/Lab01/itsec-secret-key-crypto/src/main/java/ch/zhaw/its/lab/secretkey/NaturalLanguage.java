package ch.zhaw.its.lab.secretkey;

public class NaturalLanguage {

    private final ComputeEntropy computeEntropy;

    public NaturalLanguage() {
        computeEntropy = new ComputeEntropy();
    }

    public static void main(String[] args) {
        final NaturalLanguage nl = new NaturalLanguage();
        FileHelper.doForFiles(args, result -> {
            System.out.println(String.format("File '%s' is %snatural", result.getKey(), nl.isNaturalLanguage(result.getValue()) ? "" : "not "));
        });
    }

    public boolean isNaturalLanguage(byte[] in) {
        double entropy = computeEntropy.computeEntropyPerByte(in);
        return entropy < 4.9;
    }
}
