package mediathek.tool.swing;

import javax.swing.plaf.nimbus.NimbusLookAndFeel;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

public enum LookAndFeelType {
    Nimbus(NimbusLookAndFeel.class.getName()),
    Windows("com.sun.java.swing.plaf.windows.WindowsLookAndFeel"),
    UNKNOWN("");

    private static final Map<String, LookAndFeelType> ENUM_MAP;

    static {
        Map<String, LookAndFeelType> map = new ConcurrentHashMap<>();
        for (LookAndFeelType instance : LookAndFeelType.values()) {
            map.put(instance.refClassname, instance);
        }
        ENUM_MAP = Collections.unmodifiableMap(map);
    }

    private final String refClassname;

    LookAndFeelType(String refClassname) {
        this.refClassname = refClassname;
    }

    public static LookAndFeelType get(String name) {
        var result = ENUM_MAP.get(name);
        return Objects.requireNonNullElse(result, UNKNOWN);
    }
}
