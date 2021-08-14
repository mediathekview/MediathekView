package mediathek.tool;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SingleInstanceTest {
    @Test
    void instance1_not_active() {
        try (var instance1 = new SingleInstance()) {
            assertFalse(instance1.isAppAlreadyActive());
        }
    }

    @Test
    void instance2_activity_test() {
        try (var instance1 = new SingleInstance();
        var instance2 = new SingleInstance()) {
            assertFalse(instance1.isAppAlreadyActive());
            assertTrue(instance2.isAppAlreadyActive());
        }
    }
}