package ch.zhaw.its.lab.secretkey;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

import static ch.zhaw.its.lab.secretkey.FileDecrypter.CALGORITHM;

public class MysteryDecrypter {

    private static final String FILENAME_DECRYPTED = "%s_decrypted.txt";
    private final String filename;
    private final FileDecrypter fileDecrypter;
    private final FileEncrypter fileEncrypter;
    private final byte[] encrypted;
    private final NaturalLanguage naturalLanguage;

    public MysteryDecrypter(String filename, byte[] encrypted) {
        this.filename = filename;
        fileDecrypter = new FileDecrypter(encrypted);
        fileEncrypter = new FileEncrypter(filename, String.format(FILENAME_DECRYPTED, filename));
        this.encrypted = encrypted;
        naturalLanguage = new NaturalLanguage();
    }

    public static void main(String[] args) {
        final String[] filenames = new String[args.length == 0 ? 1 : args.length];
        System.arraycopy(args, 0, filenames, 0, args.length);
        if (filenames.length == 1 && args.length == 0) {
            filenames[0] = "mystery";
        }
        FileHelper.doForFiles(filenames, result -> new MysteryDecrypter(result.getKey(), result.getValue()).decrypt());
    }

    private void decrypt() {
        try {
            byte[] key = Arrays.copyOfRange(encrypted, 0, Cipher.getInstance(CALGORITHM).getBlockSize());

            int count = 0;
            while (key[0] > Byte.MIN_VALUE) {
                key[0]--;
                count++;
                byte[] decrypted = fileDecrypter.decrypt(key);
                if (naturalLanguage.isNaturalLanguage(decrypted)) {
                    fileEncrypter.decrypt(key);
                    StringBuilder output = new StringBuilder(String.format("Key [%s] within %d tries: | ", filename, count));
                    for (byte b : key) {
                        output.append(b).append(" | ");
                    }
                    System.out.println(output.toString());

                    return;
                }

            }
        } catch (NoSuchAlgorithmException | IOException | BadPaddingException | IllegalBlockSizeException | InvalidAlgorithmParameterException | InvalidKeyException | NoSuchPaddingException e) {
            e.printStackTrace();
        }
    }
}
