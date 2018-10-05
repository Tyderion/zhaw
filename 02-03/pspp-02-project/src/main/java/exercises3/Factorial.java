package exercises3;

import de.inetsoftware.jwebassembly.api.annotation.Export;
import de.inetsoftware.jwebassembly.api.annotation.Import;
import de.inetsoftware.jwebassembly.*;

public class Factorial {
    @Export
    static int factorial(int n) {
        if (n == 0)
            return 1;
        else
            return n * factorial(n-1);
    }
}