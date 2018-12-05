package exercises3;

import de.inetsoftware.jwebassembly.JWebAssembly;
import de.inetsoftware.jwebassembly.module.*;

/**
 * User: Karl Rege
 */


class Calculator implements Emitter {
    static void expr() throws Exception {
        term();
        while (exercises3.Scanner.la == exercises3.Token.PLUS
                || exercises3.Scanner.la == exercises3.Token.MINUS) {
            exercises3.Scanner.scan();
            int op = exercises3.Scanner.token.kind;
            term();
            if (op == exercises3.Token.PLUS) {
                System.out.println("Emitting Add");
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.f64, 0));
            } else {
                System.out.println("Emitting Sub");
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.sub, ValueType.f64, 0));
            }
        }

    }

    static void term() throws Exception {
        factor();
        while (exercises3.Scanner.la == exercises3.Token.TIMES || exercises3.Scanner.la == exercises3.Token.SLASH) {
            exercises3.Scanner.scan();
            int op = exercises3.Scanner.token.kind;
            factor();
            if (op == exercises3.Token.TIMES) {
                System.out.println("Emitting Mul");
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.mul, ValueType.f64, 0));
            } else {
                System.out.println("Emitting Div");
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.div, ValueType.f64, 0));
            }
        }
    }

    static void factor() throws Exception {
        if (exercises3.Scanner.la == exercises3.Token.LBRACK) {
            exercises3.Scanner.scan();
            expr();
            exercises3.Scanner.check(exercises3.Token.RBRACK);
        } else if (exercises3.Scanner.la == exercises3.Token.NUMBER) {
            exercises3.Scanner.scan();
            System.out.println("Emitting Const");
            JWebAssembly.il.add(new WasmConstInstruction(exercises3.Scanner.token.val, 0));
        }
    }


    public static void main(String[] args) throws Exception {
        Scanner.init("1 + 1");
        Scanner.scan();
        JWebAssembly.emitCode(ICalculator.class, new Calculator ());
    }

    @Override
    public void emit() {
        try {
            expr();
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }
}

interface ICalculator {
    double calculate();
}
