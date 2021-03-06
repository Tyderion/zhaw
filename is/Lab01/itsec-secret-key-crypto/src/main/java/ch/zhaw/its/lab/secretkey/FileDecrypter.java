package ch.zhaw.its.lab.secretkey;

import javax.crypto.*;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.*;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

public class FileDecrypter {
    public static final String KALGORITHM = "AES";
    public static final String CALGORITHM = KALGORITHM + "/CBC/PKCS5Padding";
    private final byte[] bytes;

    public FileDecrypter(byte[] bytes) {
        this.bytes = bytes;
    }

    public byte[] decrypt(byte[] rawKey) throws NoSuchPaddingException, NoSuchAlgorithmException, IOException,
            BadPaddingException, IllegalBlockSizeException, InvalidAlgorithmParameterException, InvalidKeyException {
        SecretKey key = new SecretKeySpec(rawKey, 0, rawKey.length, KALGORITHM);
        Cipher cipher = Cipher.getInstance(CALGORITHM);

        IvParameterSpec ivParameterSpec = readIv(cipher);

        cipher.init(Cipher.DECRYPT_MODE, key, ivParameterSpec);
        return decrypt(new ByteArrayInputStream(bytes), cipher);
    }

    private IvParameterSpec readIv(Cipher cipher) {
        return new IvParameterSpec(Arrays.copyOfRange(bytes, 0, cipher.getBlockSize()));
    }
    
    private byte[] decrypt(InputStream is, Cipher cipher) throws IOException {

        try (ByteArrayOutputStream os = new ByteArrayOutputStream()) {
            crypt(is, os, cipher);
            return os.toByteArray();
        } catch (BadPaddingException | IllegalBlockSizeException e) {
            return new byte[0];
        }
    }

    private void crypt(InputStream is, OutputStream os, Cipher cipher) throws IOException, BadPaddingException, IllegalBlockSizeException {
        boolean more = true;
        byte[] input = new byte[cipher.getBlockSize()];

        while (more) {
            int inBytes = is.read(input);

            if (inBytes > 0) {
                os.write(cipher.update(input, 0, inBytes));
            } else {
                more = false;
            }
        }
        try {
            os.write(cipher.doFinal());
        } catch (IOException | IllegalBlockSizeException | BadPaddingException e) {
        }
    }
}
