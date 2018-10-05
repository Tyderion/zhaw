package exercises3;

import de.inetsoftware.jwebassembly.JWebAssembly;
import de.inetsoftware.jwebassembly.module.*;

import java.util.HashMap;
import java.util.Map;

/**
 * User: Karl Rege
 */


class Program implements Emitter {
    static void expr() throws Exception {
        term();
        while (Scanner.la == Token.PLUS
                || Scanner.la == Token.MINUS) {
            Scanner.scan();
            int op = Scanner.token.kind;
            term();
            if (op == Token.PLUS) {
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
        while (Scanner.la == Token.TIMES || Scanner.la == Token.SLASH) {
            Scanner.scan();
            int op = Scanner.token.kind;
            factor();
            if (op == Token.TIMES) {
                System.out.println("Emitting Mul");
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.mul, ValueType.f64, 0));
            } else {
                System.out.println("Emitting Div");
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.div, ValueType.f64, 0));
            }
        }
    }

    static void factor() throws Exception {
        if (Scanner.la == Token.LBRACK) {
            Scanner.scan();
            expr();
            Scanner.check(Token.RBRACK);
        } else if (Scanner.la == Token.NUMBER) {
            Scanner.scan();
            System.out.println("Emitting Const: " + Scanner.token.val);
            JWebAssembly.il.add(new WasmConstInstruction(Scanner.token.val, 0));
        } else if (Scanner.la == Token.IDENT) {
            Scanner.scan();
            System.out.println("Emitting Load Variable " + Scanner.token.str);
            JWebAssembly.il.add(new WasmLoadStoreInstruction(true,
                    JWebAssembly.local(variables.getOrDefault(Scanner.token.str, ValueType.f64), Scanner.token.str), 0));
        }
    }

    private static Map<String, ValueType> variables = new HashMap<>();

    static void assigment() throws Exception {
        if (Scanner.token.kind != Token.IDENT || Scanner.la != Token.EQUAL) {
            throw new Exception("Assigment must start with ident '='");
        }
        String name = Scanner.token.str;
        variables.put(name, ValueType.f64);
        Scanner.scan();
        expr();
        Scanner.check(Token.SCOLON);
        Scanner.scan();
        System.out.println("Emitting Variable " + name);
        JWebAssembly.il.add(new WasmLoadStoreInstruction(false, JWebAssembly.local(ValueType.f64, name), 0));
    }

    static void statement() throws Exception {
        assigment();
    }

    static void statementSequence() throws Exception {
        Scanner.scan();
        while (Scanner.token.kind != Token.EOF) {
            statement();
        }

        System.out.println("Emitting Load variable 'value'");
        JWebAssembly.il.add(new WasmLoadStoreInstruction(true, JWebAssembly.local(ValueType.f64, "value"), 0));

        System.out.println("Emitting return");
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
    }

    @Override
    public void emit() {
        try {
            statementSequence();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) throws Exception {
        Scanner.init("x = $arg0;" +
                "a = 1;" +
                "b = 2;" +
                "c = 3;" +
                "value = a*x*x + b*x + c;");
        Scanner.scan();
        JWebAssembly.emitCode(IProgram.class, new Program());
    }


}

interface IProgram {
    double run(double arg);
}
