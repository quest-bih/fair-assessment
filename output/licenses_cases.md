# Auffindbarkeit von Lizenzen durch Assessment Tools

F-UJI und FAIR-Enough Ansätze sind ähnlich - beide Tools suchen nach spezifischen Metadaten-Stellen (Key-Values) mit relevanten Informationen auf der Datensatz-Seite. Für die Menschen sind Metadaten über die Export-Funktion wie 'Export in DC/Datacite' bei [figshare](https://figshare.com/) auffindbar (**human readability**). Anders als die Meta-tags der Seite, die weniger menschenlesbar sind, aber gut für die Maschinen wie ein SEO-Crawler geeignet sind (**machine readability**). Die FAIR-Assessment-Tool evaluieren die Qualität der Datensätze auf FAIRness von der Seite der Maschinenlesbarkeit.

In diesem Test wurde das FAIR-Prinzip [R1.1](https://www.go-fair.org/fair-principles/r1-1-metadata-released-clear-accessible-data-usage-license/) mit Hilfe von beiden Assessment Tools durchgeführt. Im folgenden werden die Vergleiche interpretiert. 

## F-UJI
F-UJI erkennt 48 Datensätze (von 301) mit Lizenzen oder im Public Domain.

### Wie werden die Lizenz-Metadaten gefunden?

F-UJI basiert darauf, dass es die Metadaten eines Datensatzes mit den 'FAIR-enabling Services' abgleicht, wie SPDX-Lizenzenliste in diesem Fall.

'The test associated with the metric FsF-R1.1-01M checks if a data object's license is specified using an appropriate metadata field. It uses the SPDX License List to verify the name and the type of the license specified.' ([Devaraju, Huber, 2021](10.1016/j.patter.2021.100370)).

## FAIR-Enough
FAIR-Enough erkennt 41 Datensätze (von 301) mit Lizenzen oder im Public Domain.

### Wie werden die Lizenz-Metadaten gefunden?

FAIR-Enough wird als eine Kombination von [FAIR-evaluator](https://fairsharing.github.io/FAIR-Evaluator-FrontEnd/#!/) und [F-UJI](https://www.f-uji.net/) dargestellt. FAIR-Enough soll das Problem der fehlenden Flexibilität bei F-UJI lösen, indem mehrere Evaluierungen ('assessment collections') erstellt werden können, sowie von vielen Communities anhand ihrer Bedürfnisse angepasst werden.

Die Seiten von Datensätzen werden auf MIME TYPE geprüft, um die Key-Values zu finden. Folgende mime types sind akzeptabel:
![mime types fair-enough](./fair_assessment/FAIR-Enough.png) (https://github.com/MaastrichtU-IDS/fair-enough/blob/8d75f976f2fb4ea8544cf9bbedb3242a2dde070c/backend/app/utils.py).

z.B. [Dryad-Datensätze](https://datadryad.org/stash) haben den Mime Type JSON-LD ('script type="application/ld+json').

Genutzte Spezifikationen/Empfehlungen in der Test-Datensatzliste: RDFa, Opengraph, Microdata, Durlin Core, JSON-LD, 'identifier_in_matadata'

## F-UJI vs. FAIR-Enough
290 Datensätze (von 301) bei F-UJI und FAIR-Enough sind übereinstimmig.

- FAIR-Enough ist manchmal schlechter bei der Performance im **json-ld** Format.
- F-UJI gibt ausschließlich **CC-BY, CC0 oder Public Domain** Lizenzen aus; alle anderen Lizenzen sind **other licenses** kategorisiert.

Die Uneinstimmigkeiten wurden manuell geprüft. Die Ergebnisse dieser Evaluierung werden im folgenden dargestellt:

1. **Mendeley**: 
- Lizenz in **json-ld** Format
- F-UJI richtig; 
- FAIR-Enough - nicht klar, warum nicht entdeckt.
2. **Open Science Framework**:
- In 5 von 21 Datensätzen keine Übereinstimmung;
- 4 von diesen Datensätzen haben **kein DOI**;
- Alle Datensätze haben Lizenz in **dc** Format
- Metadaten-Lizenz Feld vorhanden, nicht immer ausgefüllt;
- F-UJI richtig; 
- FAIR-Enough - nicht klar, warum nicht entdeckt.
3. **CodeOcean**
- ohne DOI weder durch FUJI, noch durch FAIR-Enough die separate Metadaten-Datei gefunden;
- Mit DOI erfolgreich über FAIR-Enough;
- F-UJI voraussichtlich auch ('other license').
- Lizenz in **json-ld**, **identifier_in_metadata** und **schema.org** Formaten
4. **GIN**:
- Lizenz in **json-ld**, **identifier_in_metadata** und **schema.org** Formaten
- F-UJI *other license*;
- FAIR-Enough - richtig.
5. **PhysioNet**:
- Lizenz in **json-ld** Format
- F-UJI richtig;
- FAIR-Enough - nicht klar, warum nicht entdeckt.

Zenodo und The Electron Microscopy Data Bank - URL für FAIR-Enough ungültig.

## Andere Fälle, wenn kein Tool entdeckt Lizenz

- Gute Performance in folgenden Repositorien: **tu datalib, zenodo**
- *Terms of use* wie erwartet: **Emdb, arrayexpress, bioproject, ena, sra, european genome-phenome archive, geo, metabolights, ncbi nucleotide, synapse, wwpdb**
- Die Tools sind noch geringfähig mit dem Scannen von separaten Metadaten-Dateien (**CodeOcean, OpenNeuro, github**). 
- keine Metadaten (?): **addgene**

Manuelle Stichprobe hat gezeigt, dass die Lizenzen in manchen Fällen weder durch F-UJI, noch durch FAIR-Enough entdeckt werden:

1. **Figshare**:
- Der gleiche Datensatz kann über DOI und ohne DOI aufgerufen werden. Über DOI führt es direkt zum Download, deshalb sind die Metadaten in diesem Fall begrenzt. Wenn man denselben Datensatz ohne DOI aufruft, sind die Metadaten umfangreicher (inkl. Lizenz).
- keine Lizenzen für Nicht-DOI gefunden - MIME-Type json-ld akzeptabel, aber keine Entscheidung fürs Lizenzvorhandensein - **warum**?

2. **Harvard**: 
- in JSON-LD Lizenz Information vorhanden, im Output-Dokumentation von FAIR-Enough ist es vorhanden, aber die Entscheidung ist *"Could not find license information in metadata"* - **warum**? 
  
3. **Global Health Data Exchange**:
- keine Metadaten, obwohl die Lizenz human readable ist
  
4. **CodeOcean, OpenNeuro, Github**: 
- Lizenzvergabe in separater Datei - durch Tools nicht gefunden
 



