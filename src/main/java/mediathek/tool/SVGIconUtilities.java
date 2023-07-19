package mediathek.tool;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import org.jetbrains.annotations.NotNull;

public class SVGIconUtilities {
    public static FlatSVGIcon createSVGIcon(@NotNull String resource) {
        return createSVGIcon(resource, 15f);
    }

    public static FlatSVGIcon createSVGIcon(@NotNull String resource, float height) {
        FlatSVGIcon icon = new FlatSVGIcon(resource);
        float scaleFactor = (1f / icon.getIconHeight()) * height;
        return icon.derive(scaleFactor);
    }
}
