package ch.zhaw.its.lab.secretkey;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import java.io.*;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

public class NaturalLanguage {

    private final ComputeEntropy computeEntropy;
    private final FileDecrypter fileDecrypter;

    public NaturalLanguage(byte[] input) {
        computeEntropy = new ComputeEntropy();
        fileDecrypter = new FileDecrypter(input);
    }

    public static void mainOld(String[] args) {

    }


    public static void main(String[] args) {
        for (int i = 0; i < 1; i++) {
            byte[] rawIv = new byte[16];
            new TotallySecureRandom().nextBytes(rawIv);
            StringBuilder sb = new StringBuilder();
            for (byte b : rawIv) {
                sb.append(b).append(",");
            }
            System.out.println(sb.toString());
        }
//        FileHelper.doForFiles(args, result -> {
//            NaturalLanguage nl = new NaturalLanguage(result.getValue());
//            System.out.println(String.format("File '%s' is %snatural", result.getKey(), nl.isNaturalLanguage(result.getValue()) ? "" : "not "));
//        });
        if (args.length != 1) {
            return;
        }
        final byte[][] encrypted = new byte[1][1];
        FileHelper.doForFiles(args, result -> {
            encrypted[0] = result.getValue();
        });

//        int a = 0xb47c361669010000;

        byte[] key = new byte[] {Byte.MIN_VALUE, Byte.MIN_VALUE, Byte.MIN_VALUE, 0x16, 0x69, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

        NaturalLanguage nl = new NaturalLanguage(encrypted[0]);
        long count = 0;
        for (int i0 = Byte.MIN_VALUE; i0 <= Byte.MAX_VALUE; i0++) {
            for (int i1 = Byte.MIN_VALUE; i1 <= Byte.MAX_VALUE; i1++) {
                for (int i2 = Byte.MIN_VALUE; i2 <= Byte.MAX_VALUE; i2++) {
//                    for (int i3 = Byte.MIN_VALUE; i3 <= Byte.MAX_VALUE; i3++) {
//                        for (int i4 = Byte.MIN_VALUE; i4 <= Byte.MAX_VALUE; i4++) {
                            count++;
                            if (nl.tryDecrypt(key, count)) {
                                System.out.println("Tried " + count + " times.");
                            }
                            key[4]++;
//                        }
//                        key[4] = Byte.MIN_VALUE;
//                        key[3]++;
//                    }
//                    key[3] = Byte.MIN_VALUE;
                    key[2]++;
                }
                key[2] = Byte.MIN_VALUE;
                key[1]++;
            }
            key[1] = Byte.MIN_VALUE;
            key[0]++;
        }
        System.out.println("Exhausted keys: " + count);

    }

    private boolean isNaturalLanguage(byte[] in) {
        double entropy = computeEntropy.computeEntropyPerByte(in);
        return entropy < 4.9;
    }

    private boolean tryDecrypt(byte[] rawKey, long number) {
        byte[] decrypted = tryKey(rawKey);
        if (isNaturalLanguage(decrypted)) {
            try (FileOutputStream os = new FileOutputStream("decrypted" + number)) {
                os.write(decrypted);
            } catch (IOException e) {
                e.printStackTrace();
            }
            return true;
        }
        return false;
    }

    private byte[] tryKey(byte[] rawKey) {
        try {
            return this.fileDecrypter.decrypt(rawKey);
        } catch (NoSuchPaddingException | NoSuchAlgorithmException | IOException | BadPaddingException | IllegalBlockSizeException | InvalidKeyException | InvalidAlgorithmParameterException e) {
            e.printStackTrace();
            return new byte[0];
        }
    }
}
