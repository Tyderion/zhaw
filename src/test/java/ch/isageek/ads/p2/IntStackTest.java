package ch.isageek.ads.p2;

import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class IntStackTest {

    StackExercise stack;

    @Before
    public void setup() {
        stack = new IntStack();
    }

    @Test
    public void shouldBeEmpty() {
        assertTrue(stack.isEmpty());
    }

    @Test
    public void pushShouldAddElement() {
        stack.push(1);
        assertFalse(stack.isEmpty());
    }

    @Test
    public void popShouldReturnLastElementAndRemoveIt() {
        stack.push(1);
        assertFalse(stack.isEmpty());
        assertEquals(1, stack.pop());
        assertTrue(stack.isEmpty());
    }

    @Test
    public void topShouldShowLastElement() {
        stack.push(1);
        assertFalse(stack.isEmpty());
        assertEquals(1, stack.top());
        assertFalse(stack.isEmpty());
    }

    @Test
    public void shouldGrowStore() throws Exception{
        for (int i = 0; i < 15; i++) {
            stack.push(i);
        }
        assertFalse(stack.isEmpty());
        assertEquals(14, stack.top());
        Field storeF = IntStack.class.getDeclaredField("store");
        storeF.setAccessible(true);
        int[] store = (int[]) storeF.get(stack);
        assertEquals(20, store.length);
    }

    @Test
    public void shouldShrinkStore() throws Exception {
        for (int i = 0; i < 15; i++) {
            stack.push(i);
        }
        for (int i = 0; i < 15; i++) {
            stack.pop();
        }
        Field storeF = IntStack.class.getDeclaredField("store");
        storeF.setAccessible(true);
        int[] store = (int[]) storeF.get(stack);
        assertEquals(10, store.length);
    }
}
