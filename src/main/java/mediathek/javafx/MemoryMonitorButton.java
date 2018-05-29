package mediathek.javafx;

import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

public class MemoryMonitorButton extends Button {
    private GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");
    private MemoryMonitor memoryMonitor;

    public MemoryMonitorButton(MemoryMonitor memoryMonitor) {
        super("");
        this.memoryMonitor = memoryMonitor;

        setGraphic(fontAwesome.create(FontAwesome.Glyph.RANDOM));
        setTooltip(new Tooltip("Show memory monitor"));
        setOnAction(e -> performAction());
    }

    private void performAction() {
        if (memoryMonitor == null) {
            memoryMonitor = new MemoryMonitor();
            memoryMonitor.show();
        } else
            memoryMonitor.show();
    }
}
