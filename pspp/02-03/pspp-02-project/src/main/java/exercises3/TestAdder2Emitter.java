package exercises3;

import de.inetsoftware.jwebassembly.JWebAssembly;
import de.inetsoftware.jwebassembly.module.*;

class TestAdder2Emitter implements Emitter {
    public void emit() {
// 1 + arg0 + arg1 as double
        // load int const 1 on stack
        String name = "$arg0";
        System.out.println("Emitting Load Variable " + name);
        JWebAssembly.il.add(new WasmLoadStoreInstruction(true,
                JWebAssembly.local(ValueType.f64, name), 0));
        JWebAssembly.il.add(new WasmLoadStoreInstruction(true,
                JWebAssembly.local(ValueType.f64, "$arg1"), 0));
// add it
        JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.f64, 0));
//        JWebAssembly.il.add(new WasmLoadStoreInstruction(true,
//                JWebAssembly.local(ValueType.f64, "$arg1"), 0));
//// add it
//        JWebAssembly.il.add(new WasmNumericInstruction(NumericOperator.add, ValueType.f64, 0));
// result top of stack
        JWebAssembly.il.add(new WasmBlockInstruction(WasmBlockOperator.RETURN, null, 0));
    }

    public static void main(String[] args) throws Exception {
        JWebAssembly.emitCode(TestAdder2.class, new TestAdder2Emitter());
    }
}

interface TestAdder2 {
    double add(double a, double b);
}