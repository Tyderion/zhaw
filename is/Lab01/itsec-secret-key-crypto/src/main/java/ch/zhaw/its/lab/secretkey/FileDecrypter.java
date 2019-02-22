package ch.zhaw.its.lab.secretkey;

import javax.crypto.*;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.*;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;

public class FileDecrypter {
    public static final String KALGORITHM = "AES";
    public static final String CALGORITHM = KALGORITHM + "/CBC/PKCS5Padding";
    private final byte[] bytes;

    public FileDecrypter(byte[] bytes) {
        this.bytes = bytes;
    }

    public IvParameterSpec readIv(InputStream is, Cipher cipher) throws IOException {
        byte[] rawIv = new byte[cipher.getBlockSize()];
        int inBytes = is.read(rawIv);

        if (inBytes != cipher.getBlockSize()) {
            throw new IOException("can't read IV from file");
        }

        return new IvParameterSpec(rawIv);
    }


    public byte[] decrypt(byte[] rawKey) throws NoSuchPaddingException, NoSuchAlgorithmException, IOException,
            BadPaddingException, IllegalBlockSizeException, InvalidAlgorithmParameterException, InvalidKeyException {
        SecretKey key = new SecretKeySpec(rawKey, 0, rawKey.length, KALGORITHM);
        Cipher cipher = Cipher.getInstance(CALGORITHM);

        try (
                InputStream is = new ByteArrayInputStream(bytes)) {
            IvParameterSpec ivParameterSpec = readIv(is, cipher);

            cipher.init(Cipher.DECRYPT_MODE, key, ivParameterSpec);
            return decrypt(is, cipher);
        }
    }

    private byte[] decrypt(InputStream is, Cipher cipher) throws IOException {
        boolean more = true;
        byte[] input = new byte[cipher.getBlockSize()];

        try (ByteArrayOutputStream os = new ByteArrayOutputStream()) {
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
//                e.printStackTrace();
                return os.toByteArray();
            }
            return os.toByteArray();
        }
    }
}
