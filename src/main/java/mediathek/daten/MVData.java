package mediathek.daten;

import org.jetbrains.annotations.NotNull;

public abstract class MVData<E> implements Comparable<E> {

    @Override
    public int compareTo(@NotNull E o) {
        return 0;
    }
}
