/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList

/**
 * The base model object for all available senders that the client can process.
 */
object SenderListBoxModel {
    @JvmStatic
    val providedSenderList: EventList<String> = BasicEventList()

    init {
        providedSenderList.add("3Sat")
        providedSenderList.add("ARD")
        providedSenderList.add("ARD-alpha")
        providedSenderList.add("ARTE.DE")
        providedSenderList.add("ARTE.EN")
        providedSenderList.add("ARTE.ES")
        providedSenderList.add("ARTE.FR")
        providedSenderList.add("ARTE.IT")
        providedSenderList.add("ARTE.PL")
        providedSenderList.add("BR")
        providedSenderList.add("DW")
        providedSenderList.add("Funk.net")
        providedSenderList.add("HR")
        providedSenderList.add("KiKA")
        providedSenderList.add("MDR")
        providedSenderList.add("NDR")
        providedSenderList.add("ONE")
        providedSenderList.add("ORF")
        providedSenderList.add("PHOENIX")
        providedSenderList.add("Radio Bremen TV")
        providedSenderList.add("RBB")
        providedSenderList.add("SR")
        providedSenderList.add("SRF")
        //providedSenderList.add("SRF.Podcast")
        providedSenderList.add("tagesschau24")
        providedSenderList.add("SWR")
        providedSenderList.add("WDR")
        providedSenderList.add("ZDF")
        providedSenderList.add("ZDFinfo")
        providedSenderList.add("ZDFneo")
        providedSenderList.add("ZDF-tivi")
        providedSenderList.sortWith(GermanStringSorter.getInstance())
    }
}