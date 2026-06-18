package mediathek.mainwindow

import java.util.function.BooleanSupplier
import java.util.function.Supplier
import javax.swing.Action
import javax.swing.Icon
import javax.swing.JComponent

class MainWindowTab @JvmOverloads constructor(
    val title: String,
    val component: JComponent,
    val visible: BooleanSupplier = BooleanSupplier { true },
    val icon: Supplier<Icon?>? = null,
    val toggleAction: Action? = null,
    val dispose: Runnable = Runnable {},
)
