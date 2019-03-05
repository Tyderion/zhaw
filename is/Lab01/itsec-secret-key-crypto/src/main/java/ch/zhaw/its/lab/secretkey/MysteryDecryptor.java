package ch.zhaw.its.lab.secretkey;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.IvParameterSpec;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;

import static ch.zhaw.its.lab.secretkey.FileDecrypter.CALGORITHM;

public class MysteryDecryptor {

    private final static String FILENAME_DECRYPTED = "decrypted.txt";
    private final FileDecrypter fileDecrypter;
    private final NaturalLanguage naturalLanguage;

    public MysteryDecryptor(byte[] encrypted) {
        fileDecrypter = new FileDecrypter(encrypted);
        naturalLanguage = new NaturalLanguage();
    }

    public static void main(String[] args) {
        if (args.length != 1) {
            return;
        }
        FileHelper.doForFiles(args, result -> {

            new MysteryDecryptor(result.getValue()).decrypt();
        });
    }

    private void decrypt() {
        try {
            IvParameterSpec iv = fileDecrypter.readIv(Cipher.getInstance(CALGORITHM));
            byte[] key = iv.getIV();
            for (byte j = Byte.MIN_VALUE; j < Byte.MAX_VALUE; j++) {
                key[0] = j;
                byte[] decrypted = fileDecrypter.decrypt(key);
                if (naturalLanguage.isNaturalLanguage(decrypted)) {
                    writeFile(FILENAME_DECRYPTED, decrypted);
                }
            }
        } catch (NoSuchAlgorithmException | IOException | BadPaddingException | IllegalBlockSizeException | InvalidAlgorithmParameterException | InvalidKeyException | NoSuchPaddingException e) {
            e.printStackTrace();
        }
    }

    private void writeFile(String name, byte[] content) {
        try (FileOutputStream os = new FileOutputStream(name)) {
            os.write(content);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
