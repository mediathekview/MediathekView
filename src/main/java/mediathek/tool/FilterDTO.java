package mediathek.tool;

import java.util.UUID;

public record FilterDTO(UUID id, String name) {
    @Override
    public String toString() {
        return name;
    }
}
