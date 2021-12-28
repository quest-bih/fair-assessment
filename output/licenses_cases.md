
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
11.	Zeile 227 – gleich wie in 226, warum ist es **“other license”** (auch CC)?	


## 27.12.2021
1.Wie werden die Lizenz-Metadaten gefunden?
Die Seiten von Datensätzen werden auf MIME TYPE geprüft, um die Key-Values zu finden (https://github.com/MaastrichtU-IDS/fair-enough/blob/8d75f976f2fb4ea8544cf9bbedb3242a2dde070c/backend/app/utils.py).
Deshalb wird es nicht immer über 'Export in DC/Datacite' auffindbar, aber die FAIR-Tools finden es.

z.B. Dryad-Seiten haben den Type JSON-LD (<script type="application/ld+json">), dass problemlos von FAIR-Enough gefunden wird.
Ausnahmen:
2. Figshare ohne DOI: MIME-Type akzeptabel, aber keine Auffinbarkeit - **warum**?

3. Harvard (Zeile 47): in JSON-LD Lizenz Information vorhanden, FAIR-Enough findet es, aber die Entscheidung ist "Could not find license information in metadata" - **warum**?
