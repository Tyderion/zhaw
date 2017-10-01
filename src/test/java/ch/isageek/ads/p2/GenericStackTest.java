package ch.isageek.ads.p2;

import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.math.BigDecimal;

import static org.junit.Assert.*;

public class GenericStackTest {

    GenericStackExercise<String> stack;

    @Before
    public void setup() {
        stack = new GenericStack<>();
    }

    @Test
    public void shouldBeEmpty() {
        assertTrue(stack.isEmpty());
    }

    @Test
    public void pushShouldAddElement() {
        stack.push("first element");
        assertFalse(stack.isEmpty());
    }

    @Test
    public void popShouldReturnLastElementAndRemoveIt() {
        stack.push("first element");
        assertFalse(stack.isEmpty());
        assertEquals("first element", stack.pop());
        assertTrue(stack.isEmpty());
    }

    @Test
    public void topShouldShowLastElement() {
        stack.push("first element");
        assertFalse(stack.isEmpty());
        assertEquals("first element", stack.top());
        assertFalse(stack.isEmpty());
    }

    @Test
    public void shouldWorkWithBigDecimals() {
        BigDecimal value = new BigDecimal(100);
        GenericStackExercise<BigDecimal> decimalStack = new GenericStack<>();

        assertTrue(decimalStack.isEmpty());
        decimalStack.push(value);
        assertFalse(decimalStack.isEmpty());

        assertEquals(value, decimalStack.top());
        assertFalse(decimalStack.isEmpty());

        assertEquals(value, decimalStack.pop());
        assertTrue(decimalStack.isEmpty());
    }

    @Test
    public void shouldGrowStore() {
        for (int i = 0; i < 15; i++) {
            stack.push(String.valueOf(i));
        }
        assertFalse(stack.isEmpty());
        assertEquals(String.valueOf(14), stack.top());
    }

    @Test
    public void shouldShrinkStore() throws Exception {
        for (int i = 0; i < 15; i++) {
            stack.push(String.valueOf(i));
        }
        for (int i = 0; i < 15; i++) {
            stack.pop();
        }
        Field storeF = GenericStack.class.getDeclaredField("store");
        storeF.setAccessible(true);
        Object[] store = (Object[]) storeF.get(stack);
        assertEquals(10, store.length);
    }
}
