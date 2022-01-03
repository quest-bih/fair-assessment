
Change proteomeexchange repository name on pride! it's not consistent.

1.	tu datalib + osf + zenodo + mendeley - gute Beispiele
2. Lizenz nicht von FUJI gefunden (keine machine-readable metadata?): harvard (Zeile 47), addgene (Zeile 84), codeocean (Zeile 99-101)
Metadata haben: codeocean, openneuro
3.	OSF – Metadaten-Lizenz Feld vorhanden, nicht immer ausgefüllt
4.	**Emdb, arrayexpress, github, bioproject, ena, sra, european genome-phenome archive, geo, metabolights, ncbi nucleotide, synapse, wwpdb** - terms of us wie erwartet
5.	keine Lizenzen für Nicht-DOI in figshare gefunden
6.	Figshare -> keine Metadaten über Lizenzen; andere general-purposed haben Metadaten
7.	Zeile 39 – **SRA** anstatt githib - warum?
8.	Zeile 102 – **kein Zugang** zu den Metadaten
9.	Zeile 103 – **kein Zugang** zum Datensatz
10.	Zeile 224 – cc0, aber wo ist **human-readable** Lizenz?


## 27.12.2021

Generell nicht klar, wie 'data_license_name' zustande kommt, weil es oft einen Lizenznamen hat, der nicht zustimmt -> kommt es aus re3data, dann bezieht es sich sehr wahrscheinlich auf Lizensierung der ganzen Datenbank, aber nicht des jeweiligen untersuchten Datensatzes.

Genutzte Spezifikationen/Empfehlungen: RDFa, Opengraph, Microdata, Durlin Core, JSON-LD, 'identifier_in_matadata'

### Wie werden die Lizenz-Metadaten gefunden?
Die Seiten von Datensätzen werden auf MIME TYPE geprüft, um die Key-Values zu finden (https://github.com/MaastrichtU-IDS/fair-enough/blob/8d75f976f2fb4ea8544cf9bbedb3242a2dde070c/backend/app/utils.py).
Deshalb wird es nicht immer durch Menschen über die Export-Funktion wie 'Export in DC/Datacite' auffindbar, aber die FAIR-Tools finden es.

z.B. Dryad-Seiten haben den Type JSON-LD (<script type="application/ld+json">), dass problemlos von FAIR-Enough gefunden wird.
  
Leider bedeutet es nicht, dass die Tools alles finden, was Menschen finden - sie sind noch recht schlecht mit dem Scannen von separaten Metadaten-Dateien (siehe 4, 6).
 
### Ausnahmen:

1. Figshare (Zeile 4) ohne DOI: MIME-Type akzeptabel, aber keine Auffinbarkeit - **warum**?

2. Harvard (Zeile 47): in JSON-LD Lizenz Information vorhanden, FAIR-Enough findet es, aber die Entscheidung ist "Could not find license information in metadata" - **warum**?
  
3. Mendeley (Zeile 48): FUJI richtig, FAIR-Enough falsch - in Evaluation output gibt es 'json-ld' mit der Lizenz - warum nicht gefunden?

3. OSF (Zeile 55): FAIR-Enough und FUJI keine Übereinstimmung -> FAIR-Enough ist hier schlechter
  
4. CodeOCean (Zeile 102): ohne DOI weder durch FUJI, noch durch FAIR-Enough die separate Metadaten-Datei gefunden;
  Mit DOI erfolgreich.
  
  data_license_name -> 'CC0_AND_Copyrights_AND_other' - warum CC0 hier?
  
5. Global Health Data Exchange (Zeile 222): wirklich keine Metadaten, obwohl die Lizenz human readable ist
  
6. OpenNeuro (Zeile 241): wenige Metadaten durch FAIR-Enough gefunden; separate Datei mit Metadaten, humanreadable.
