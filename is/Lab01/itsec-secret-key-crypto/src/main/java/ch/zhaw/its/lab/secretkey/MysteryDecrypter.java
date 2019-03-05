package ch.zhaw.its.lab.secretkey;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.IvParameterSpec;
import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;

import static ch.zhaw.its.lab.secretkey.FileDecrypter.CALGORITHM;

public class MysteryDecrypter {

    private static final String FILENAME_DECRYPTED = "%s_decrypted.txt";
    private final String filename;
    private final FileDecrypter fileDecrypter;
    private final FileEncrypter fileEncrypter;
    private final NaturalLanguage naturalLanguage;

    public MysteryDecrypter(String filename, byte[] encrypted) {
        this.filename = filename;
        fileDecrypter = new FileDecrypter(encrypted);
        fileEncrypter = new FileEncrypter(filename, String.format(FILENAME_DECRYPTED, filename));
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
            IvParameterSpec iv = fileDecrypter.readIv(Cipher.getInstance(CALGORITHM));
            byte[] key = iv.getIV();
            for (byte j = Byte.MIN_VALUE; j < Byte.MAX_VALUE; j++) {
                key[0] = j;
                byte[] decrypted = fileDecrypter.decrypt(key);
                if (naturalLanguage.isNaturalLanguage(decrypted)) {
                    fileEncrypter.decrypt(key);
                    StringBuilder output = new StringBuilder(String.format("Key [%s]: | ", filename));
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
