package ch.isageek.ads.p2;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class StackTest {

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
}
