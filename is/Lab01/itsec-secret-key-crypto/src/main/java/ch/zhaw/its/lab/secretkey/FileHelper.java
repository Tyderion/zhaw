package ch.zhaw.its.lab.secretkey;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.AbstractMap;
import java.util.Map;
import java.util.function.Consumer;

public class FileHelper {
    public static void doForFiles(String[] fileNames, Consumer<Map.Entry<String, byte[]>> consumer)  {
        for (String name : fileNames) {
            try (InputStream is = new FileInputStream(name)) {
                ByteArrayOutputStream bytes = new ByteArrayOutputStream();
                int in;
                while ((in = is.read()) != -1) {
                    bytes.write((byte) in);
                }
                consumer.accept(new AbstractMap.SimpleEntry<>(name, bytes.toByteArray()));
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
