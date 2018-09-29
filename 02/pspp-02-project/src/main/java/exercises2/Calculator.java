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
        }
    }


    public static void main(String[] args) throws Exception {
        Scanner.init("3+2-4*(2+ 1 *3)/(2 * (3 + 4))");
        Scanner.scan();
        expr();
        System.out.println("result=" + pop());
    }

}
