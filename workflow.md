# Code klonen
*(prev. **Code auschecken**)*

Da nur einige Teammitglieder Schreibzugriff auf das Repo haben, solltest du einen [Fork anlegen](https://github.com/mediathekview/MediathekView/fork) und diesen dann klonen, damit du deine Änderungen hochladen kannst.  
Es sollte für dieses Projekt `pull --rebase` aktiviert sein um unnötige Merge-Commits durch ein Aktualisieren zu vermeiden.  
Außerdem forciert unser Workflow Merge-Commits, "fast-forward"-Merges sind _nicht_ erlaubt.
```bash
# Zum Komplieren müssen alle Projekte in einem gemeinsamen Ordner liegen und
# MSearch wird derzeit noch als lokale Abhängigkeit benötigt.
mkdir mediathekview && cd $_
git clone https://github.com/mediathekview/MSearch.git

# Den Fork klonen
git clone https://github.com/<Dein GitHub Nutzer>/MediathekView.git
cd MediathekView

# Um das Repo einfach vom offiziellen aktualisieren zu können:
git remote add upstream https://github.com/mediathekview/MediathekView.git

# "git config" ohne --global oder --system schreiben nur für das aktuell Repo, 
# es werden also keine Einstellungen für andere Repos überschrieben.

# git pull --rebase als Standard
git config pull.rebase preserve # preserve um Merges zu erhalten.
# git merge --no-ff als Standard
git config merge.ff false
```
---
#  Mit Git Flow entwickeln
- `master` ist immer die letzte, stabilen Version,
- `develop` sind alle Änderungen für die nächste Version,
- `feature/#xyz-abc` für die Entwicklung von neuen Funktionen etc,
- `hotfix/#xyz-abc` für die Behebung kritischer Fehler,
- `feature`-Branches basieren immer auf `develop` und mergen auch wieder dorthin zurück,
- `hotfix`-Branches basieren immer auf `master` und mergen sowohl dorthin als auch auf develop zurück,
- jeder Merge auf `master` entspricht einer neuen (stabilen) Version.

Ein Feature sollte immer möglichst atomar gehalten sein, also nur eine (abstrakte) Sache hinzufügen, entfernen oder verbessern. Es sollte also bspw. nicht einer Klasse eine Methode hinzugefügt werden und einer anderen, nicht in Verbindung stehenden Klasse eine entfernt werden.

# Workflow

Öffne einen [Issue](https://github.com/mediathekview/MediathekView/issues/new), der dein (neues) Feature oder deinen Hotfix beschreibt. Warte möglw. auf Antworten um doppelte oder unnötige Arbeit zu vermeiden. Wenn dann soweit alles gut ist, kannst du mit der Entwicklung anfangen.

Wenn du das Feature in Bearbeitung hast, füge das entsprechende Tag "inBearbeitung" und dich als Assignee zum Issue hinzu.

Nach dem Abschluss der Entwicklung dann eine Pull Request öffnen, den Feature-Issue in der Beschreibung verlinken\*. Beim Issue das Tag "inBearbeitung" mit "Fertig" und Assignee mit **alex1702** ersetzen. Für einen Hotfix musst du zwei PRs erstellen, das wird weiter unten behandelt.

\* GitHub ersetzt bspw. #112 automatisch mit einem Link zum dazugehörigen Issue (oder auch PR)

## Fork / ohne Schreibzugriff
### mit `git-flow`
#### Feature
```bash
# Vor dem Starten immer aktualisieren (vom offiziellen Repo)
git pull upstream develop

# Ein Featurebranch erstellen.
# Name ist immer die Issue-Nummer, der auch ein Name angehängt werden kann
git flow feature start '#112-add-sth'

# Wörk Wörk

# Nochmals aktualisieren
git pull upstream develop

# Feature abschließen, wird automatisch hochgeladen.
git flow feature finish '#112-add-sth'
```
Jetzt wie oben beschrieben die PR erstellen, hier für `develop` von deinem Fork gegen `develop` von MediathekView.
#### Hotfix
```bash
# Vor dem Starten immer aktualisieren (vom offiziellen Repo)
git pull upstream master

# Eine Hotfixbranch erstellen.
# Name ist immer die Issue-Nummer, der auch ein Name angehängt werden kann
git hotfix start '#113-fix-sth'

# Wörk Wörk

# Nochmals aktualisieren (vom offiziellen Repo)
git pull upstream master

# Feature hochladen (zum Fork)
git push origin 'hotfix/#113-fix-sth'
```
Jetzt wie oben beschrieben _zwei_ PR für `hotfix/#113-fix-sth` erstellen, einmal gegen `master` und einmal gegen `develop`.
### ohne `git-flow`
#### Feature
```bash
# Vor dem Starten immer aktualisieren (vom offiziellen Repo)
git pull upstream develop

# Eine Featurebranch erstellen.
# Name ist immer die Issue-Nummer, der auch ein Name angehängt werden kann
git checkout -b 'feature/#112-add-sth' develop

# Wörk Wörk

# Nochmals aktualisieren (vom offiziellen Repo)
git pull upstream develop

# Feature hochladen (zum Fork)
git push origin 'feature/#112-add-sth'
```
Jetzt wie oben beschrieben eine PR für `feature/#112-add-sth` gegen `develop` auf GitHub erstellen.
#### Hotfix
```bash
# Vor dem Starten immer aktualisieren (vom offiziellen Repo)
git pull upstream master

# Eine Hotfixbranch erstellen.
# Name ist immer die Issue-Nummer, der auch ein Name angehängt werden kann
git checkout -b 'hotfix/#113-fix-sth' master

# Wörk Wörk

# Nochmals aktualisieren (vom offiziellen Repo)
git pull upstream master

# Feature hochladen (zum Fork)
git push origin 'hotfix/#113-fix-sth'
```
Jetzt wie oben beschrieben _zwei_ PR für `hotfix/#113-fix-sth` erstellen, einmal gegen `master` und einmal gegen `develop`.
