package exercises3;

import de.inetsoftware.jwebassembly.JWebAssembly;
import de.inetsoftware.jwebassembly.module.*;

class TestAdderEmitter implements Emitter {
    public void emit() {
        JWebAssembly.il.add(new WasmConstInstruction(1.0, 0));
        JWebAssembly.il.add(new WasmConstInstruction(5.5, 0));
// load int const 8 on stack
//        JWebAssembly.il.add(new WasmConstInstruction(8.0, 0));
//// add it
//        JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.f64, 0));
//// result top of stack
        JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.f64, 0));
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
    }

    public static void main(String[] args) throws Exception {
        JWebAssembly.emitCode(TestAdder.class, new TestAdderEmitter());
    }
}

interface TestAdder {
    int add();
}
