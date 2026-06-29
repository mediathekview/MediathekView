package mediathek.gui.bookmark

import mediathek.daten.ListeFilme

class BookmarkServices(allFilms: ListeFilme) {
    val list: BookmarkDataList = BookmarkDataList(allFilms)

    fun loadFromFile() {
        list.loadFromFile()
    }

    fun saveToFile() {
        list.saveToFile()
    }
}
