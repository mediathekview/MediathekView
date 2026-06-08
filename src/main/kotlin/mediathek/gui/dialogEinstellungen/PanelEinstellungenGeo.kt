package mediathek.gui.dialogEinstellungen

import com.formdev.flatlaf.util.ScaledImageIcon
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.daten.Country
import mediathek.gui.dialog.DialogHilfe
import mediathek.gui.messages.BlacklistChangedEvent
import mediathek.gui.messages.GeoStateChangedEvent
import mediathek.tool.*
import java.awt.Dimension
import javax.swing.AbstractButton
import javax.swing.ImageIcon
import javax.swing.JFrame
import kotlin.coroutines.CoroutineContext

class PanelEinstellungenGeo @JvmOverloads constructor(
    private val parentComponent: JFrame?,
    private val preselectFromPublicIp: Boolean = false,
) : PanelEinstellungenGeoBase(), CoroutineScope {
    override val coroutineContext: CoroutineContext = SupervisorJob() + Dispatchers.Swing

    private var userChangedSelection = false
    private var preselectionJob: Job? = null

    init {
        setCountryFlags()
        initializePanel()
    }

    override fun removeNotify() {
        super.removeNotify()
        if (!isDisplayable) {
            preselectionJob?.cancel()
            cancel()
        }
    }

    private fun initializePanel() {
        selectCountryButton(ApplicationConfiguration.getInstance().geographicLocation)
        installCountryActions()
        configureHelpButton()
        maybePreselectCountryFromPublicIp()
    }

    private fun setCountryFlags() {
        lblIcon_DE.icon = getScaledIconResource("/icons/countries/162-germany.png")
        lblIcon_AT.icon = getScaledIconResource("/icons/countries/003-austria.png")
        lblIcon_CH.icon = getScaledIconResource("/icons/countries/205-switzerland.png")
        lblIcon_FR.icon = getScaledIconResource("/icons/countries/195-france.png")
        lblIcon_EU.icon = getScaledIconResource("/icons/countries/259-european-union.png")
    }

    private fun getScaledIconResource(url: String): ScaledImageIcon {
        val icon = ImageIcon(requireNotNull(javaClass.getResource(url)))
        val imageDim = Dimension(icon.iconWidth, icon.iconHeight)
        val destDim = calculateFittedDimension(imageDim, FLAG_DIMENSIONS)
        return ScaledImageIcon(icon, destDim.width, destDim.height)
    }

    private fun calculateFittedDimension(imageSize: Dimension, boundary: Dimension): Dimension {
        var newWidth = imageSize.width
        var newHeight = imageSize.height

        if (imageSize.width > boundary.width) {
            newWidth = boundary.width
            newHeight = newWidth * imageSize.height / imageSize.width
        }

        if (newHeight > boundary.height) {
            newHeight = boundary.height
            newWidth = newHeight * imageSize.width / imageSize.height
        }

        return Dimension(newWidth, newHeight)
    }

    private fun installCountryActions() {
        registerCountryAction(jRadioButtonDe, Country.DE)
        registerCountryAction(radioButtonFR, Country.FR)
        registerCountryAction(jRadioButtonCH, Country.CH)
        registerCountryAction(jRadioButtonAt, Country.AT)
        registerCountryAction(jRadioButtonEu, Country.EU)
        registerCountryAction(jRadioButtonSonst, Country.OTHER)
    }

    private fun registerCountryAction(button: AbstractButton, country: Country) {
        button.addActionListener {
            userChangedSelection = true
            applyCountrySelection(country, notifyChanges = true)
        }
    }

    private fun configureHelpButton() {
        jButtonHilfe.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg")
        jButtonHilfe.addActionListener {
            DialogHilfe(parentComponent, true, GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_GEO)).isVisible = true
        }
    }

    private fun maybePreselectCountryFromPublicIp() {
        if (!preselectFromPublicIp) {
            return
        }

        preselectionJob?.cancel()
        preselectionJob = launch {
            try {
                val detectedCountry = withContext(Dispatchers.IO) {
                    GeoLocationDetector.detectCountry()
                } ?: return@launch

                if (isDisplayable && !userChangedSelection) {
                    applyCountrySelection(detectedCountry, notifyChanges = false)
                }
            } catch (_: CancellationException) {
                // Panel disposal cancels the preselection job as part of the normal lifecycle.
            } finally {
                preselectionJob = null
            }
        }
    }

    private fun applyCountrySelection(country: Country, notifyChanges: Boolean) {
        selectCountryButton(country)
        ApplicationConfiguration.getInstance().geographicLocation = country
        if (notifyChanges) {
            filterBlacklistAndNotifyChanges()
        }
    }

    private fun selectCountryButton(country: Country) {
        when (country) {
            Country.CH -> jRadioButtonCH.isSelected = true
            Country.AT -> jRadioButtonAt.isSelected = true
            Country.EU -> jRadioButtonEu.isSelected = true
            Country.FR -> radioButtonFR.isSelected = true
            Country.OTHER -> jRadioButtonSonst.isSelected = true
            else -> jRadioButtonDe.isSelected = true
        }
    }

    private fun filterBlacklistAndNotifyChanges() {
        Daten.getInstance().listeBlacklist.filterListe()
        MessageBus.messageBus.publishAsync(GeoStateChangedEvent())
        MessageBus.messageBus.publishAsync(BlacklistChangedEvent())
    }

    companion object {
        private val FLAG_DIMENSIONS = Dimension(32, 24)
    }
}
