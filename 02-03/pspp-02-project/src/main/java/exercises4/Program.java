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
                    try {
                        whileStatemnt();
                    } catch (Exception h) {
                        debug(h.getMessage());
                        block();
                    }
                }
            }
        }
    }

    static void block() throws Exception {
        if (Scanner.la != Token.LCBRACK) {
            throw new Exception("block must start with '{'");
        }
        Scanner.scan();
        debug("Emitting block start");
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.BLOCK, 0));
        statementSequence();
        Scanner.check(Token.RCBRACK);
        debug("Emitting block end");
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, 0));
    }

    static void returnStatement() throws Exception {
        if (Scanner.la != Token.RETURN) {
            throw new Exception("return must start with 'return'");
        }
        Scanner.scan();
        expr();
        Scanner.check(Token.SCOLON);
        debug("Emitting Return statement");
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
    }

    static void condition() throws Exception {
        if (Scanner.la != Token.LBRACK) {
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
        }
        debug("Emitting else block end");
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, 0));
    }


    static void whileStatemnt() throws Exception {
        if (Scanner.la != Token.WHILE) {
            throw new Exception("while must start with 'while'");
        }
        Scanner.scan();
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.LOOP, 0));
        condition();
        debug("Emitting if block start");
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.IF, 0));
        statement();
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.BR,1,0));
        debug("Emitting if block end");
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, 0));
        debug("Emitting loop block end");
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.END, 0));


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
    }

    public static void main(String[] args) throws Exception {
//        Scanner.init("a = 2" +
//                "if (a - 2) { value = 2 } else { value = 1 }");

//        Scanner.init("m = $arg0 - 42; if (!m) { return 7; } " +
//                "else { " +
//                "return 666; "
//                + "}"
//        );

        Scanner.init(
                "m = $arg0;" +
                " s = 1;" +
                " while (m) {" +
                " s = s * m;" +
                " m = m - 1;" +
                " }" +
                " return s;");

        JWebAssembly.emitCode(IProgram.class, new Program());
    }

    @Override
    public void emit() {
        try {
            Scanner.scan();
            statementSequence();



            // Emit dummy const so that there is *always* something on the stack at the end of the emitted code.
            // IF not, it can trigger a validation error when running in firefox.
            // Err: CompileError: "wasm validation error: at offset 86: reading value from empty stack"
            // Even though the program never reaches offset 86 which is after the else (see dissambly below, 0x56 = 86)
            // "m = $arg - 42; if (!m) { return 7; } else { return 666; }" triggers the error
            // "m = $arg - 42; if (!m) { return 7; }  return 666; " does not
            // Due to no statement after this one, the value is completely ignored
            /*
            000020 <run>:
             000022: 01 7c                      | local[0] type=f64
             000024: 20 00                      | get_local 0
             000026: 44 00 00 00 00 00 00 45 40 | f64.const 0x1.5p+5
             00002f: a1                         | f64.sub
             000030: 21 01                      | set_local 1
             000032: 20 01                      | get_local 1
             000034: aa                         | i32.trunc_s/f64
             000035: 41 00                      | i32.const 0
             000037: 47                         | i32.ne
             000038: 04 40                      | if
             00003a: 02 40                      |   block
             00003c: 44 00 00 00 00 00 00 1c 40 |     f64.const 0x1.cp+2
             000045: 0f                         |     return
             000046: 0b                         |   end
             000047: 05                         | else
             000048: 02 40                      |   block
             00004a: 44 00 00 00 00 00 d0 84 40 |     f64.const 0x1.4dp+9
             000053: 0f                         |     return
             000054: 0b                         |   end
             000055: 0b                         | end
             000056: 0b                         | end <-- Error here even though this is never reached
  */
            JWebAssembly.il.add(new WasmConstInstruction(0.0, 0));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


}
