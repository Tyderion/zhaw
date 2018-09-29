package exercises2;

/**
 * User: Karl Rege
 */


class Calculator {
    static double stack[] = new double[10];
    static int sp = 0;

    static void push(double val) {
        stack[sp++] = val;
    }

    static double pop() {
        return stack[--sp];
    }

    static void expr() throws Exception {
        term();
        while (Scanner.la == Token.PLUS
                || Scanner.la == Token.MINUS) {
            Scanner.scan();
            int op = Scanner.token.kind;
            term();
            if (op == Token.PLUS) {
                push(pop() + pop());
            } else {
                push(-pop() + pop());
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
                push(pop() * pop());
            } else {
                push(pop() / pop());
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
            push(Scanner.token.val);
        } else if (Scanner.la == Token.IDENT) {
            Scanner.scan();
            if (Scanner.token.str.equals("E")) {
                push(Math.E);
            } else if (Scanner.token.str.equals("PI")) {
                push(Math.PI);
            }
        }
    }


    public static void main(String[] args) throws Exception {
        Scanner.init("PI + 2 * E");
        Scanner.scan();
        expr();
        System.out.println("result=" + pop());
    }

}
