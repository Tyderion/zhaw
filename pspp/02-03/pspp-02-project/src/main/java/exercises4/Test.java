package exercises4;

import de.inetsoftware.jwebassembly.api.annotation.Export;

public class Test {

    @Export()
    public static int test() {
        int n = 2;
        int i = 0;
        if (n == 0)
            i = 1;
        else
            i = 2;
        return i;
    }
}
