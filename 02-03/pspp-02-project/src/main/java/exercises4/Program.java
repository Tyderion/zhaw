package exercises4;

import de.inetsoftware.jwebassembly.JWebAssembly;
import de.inetsoftware.jwebassembly.module.*;
import exercises3.Token;

import java.util.HashMap;
import java.util.Map;

interface IProgram {
    double run(double arg);
}

/**
 * User: Karl Rege
 */


class Program implements Emitter {
    private static boolean debug = true;
    private static Map<String, ValueType> variables = new HashMap<>();

    static void expr() throws Exception {
        term();
        while (Scanner.la == Token.PLUS
                || Scanner.la == Token.MINUS) {
            Scanner.scan();
            int op = Scanner.token.kind;
            term();
            if (op == Token.PLUS) {
                debug("Emitting Add");
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.f64, 0));
            } else {
                debug("Emitting Sub");
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.sub, ValueType.f64, 0));
            }
        }

    }

    private static void debug(String value) {
        if (debug) {
            System.out.println(value);
        }
    }

    static void term() throws Exception {
        factor();
        while (Scanner.la == Token.TIMES || Scanner.la == Token.SLASH) {
            Scanner.scan();
            int op = Scanner.token.kind;
            factor();
            if (op == Token.TIMES) {
                debug("Emitting Mul");
                JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.mul, ValueType.f64, 0));
            } else {
                debug("Emitting Div");
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
            debug("Emitting Const: " + Scanner.token.val);
            JWebAssembly.il.add(new WasmConstInstruction(Scanner.token.val, 0));
        } else if (Scanner.la == Token.IDENT) {
            Scanner.scan();
            debug("Emitting Load Variable " + Scanner.token.str);
            JWebAssembly.il.add(new WasmLoadStoreInstruction(true,
                    JWebAssembly.local(variables.getOrDefault(Scanner.token.str, ValueType.f64), Scanner.token.str), 0));
        }
    }

    static void assigment() throws Exception {
        if (Scanner.la != Token.IDENT) {
            throw new Exception("Assigment must start with 'ident'");
        }
        Scanner.scan();
        if (Scanner.la != Token.EQUAL) {
            throw new Exception("Assigment must start with \"ident '='\"");
        }
        String name = Scanner.token.str;
        variables.put(name, ValueType.f64);
        Scanner.scan();
        expr();
        Scanner.check(Token.SCOLON);
//        Scanner.scan();
        debug("Emitting Variable " + name);
        JWebAssembly.il.add(new WasmLoadStoreInstruction(false, JWebAssembly.local(ValueType.f64, name), 0));
    }

    static void statement() throws Exception {
        try {
            assigment();
        } catch (Exception e) {
            debug(e.getMessage());
            try {
                returnStatement();
            } catch (Exception f) {
                debug(f.getMessage());
                try {
                    ifStatement();
                } catch (Exception g) {
                    debug(g.getMessage());
                    block();
                }
            }
        }
    }

    static void block() throws Exception {
        if (Scanner.la != Token.LCBRACK ) {
            throw new Exception("block must start with '{'");
        }
        Scanner.scan();
        debug("Emitting block start");
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.BLOCK, 0));
        statementSequence();
        Scanner.check(Token.RCBRACK);
        debug("Emitting block end");
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, 0));
//        Scanner.scan();
//        Scanner.scan();

    }

    static void returnStatement() throws Exception {
        if (Scanner.la != Token.RETURN) {
            throw new Exception("return must start with 'return'");
        }
        Scanner.scan();
        expr();
        Scanner.check(Token.SCOLON);
        Scanner.scan();
        debug("Emitting Return statement");
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
    }

    static void condition() throws Exception {
        if (Scanner.la != Token.LBRACK ) {
            throw new Exception("condition must start with '('");
        }
        Scanner.scan();
        boolean inverted = false;
        if (Scanner.la == Token.NOT) {
            Scanner.scan();
            inverted = true;
        }
        expr();
        debug("Emitting convert instruction");
        JWebAssembly.il.add(new WasmConvertInstruction(ValueTypeConvertion.d2i, 0));
        JWebAssembly.il.add(new WasmConstInstruction(0, 0));
        if (inverted) {
            debug("Emitting not-equal");
            JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.eq, ValueType.i32, 0));
        } else {
            debug("Emitting equal");
            JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.ne, ValueType.i32, 0));
        }
        Scanner.check(Token.RBRACK);
    }

    static void ifStatement() throws Exception {
        if (Scanner.la != Token.IF) {
            throw new Exception("if must start with 'if'");
        }
        Scanner.scan();
        condition();
        debug("Emitting if block start");
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.IF, 0));
        statement();
        if (Scanner.la == Token.ELSE) {
            Scanner.scan();
            debug("Emitting else block start");
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.ELSE, 0));
            statement();
            debug("Emitting else block end");
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, 0));
        } else {
            debug("Emitting if block end");
            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, 0));
        }
    }


    static void whileStatemnt() throws Exception {

    }


    static void statementSequence() throws Exception {
        while (Scanner.token == null || Scanner.token.kind != Token.EOF && Scanner.la != Token.RCBRACK) {
            try {
                statement();
            } catch (Exception e) {
                debug(e.getMessage());
                break;
            }
        }

//        Scanner.scan();

//        debug("Emitting Load variable 'value'");
//        JWebAssembly.il.add(new WasmLoadStoreInstruction(true, JWebAssembly.local(ValueType.f64, "value"), 0));
    }

    public static void main(String[] args) throws Exception {
//        Scanner.init("a = 2" +
//                "if (a - 2) { value = 2 } else { value = 1 }");

        Scanner.init("n = $arg0; i = 15;"+
                "if (n) i = 1;" +
                "else i = 2;" +
                "return i;");


//        Scanner.init("i = 5; " +
//                "if (i-5) { i = 15; }" +
//                "return i;");

//        Scanner.init("return 1;");
        JWebAssembly.emitCode(IProgram.class, new Program());

//        Test.test();
    }

    @Override
    public void emit() {
        try {

//            JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.IF, 0));

            Scanner.scan();
            statementSequence();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


}
