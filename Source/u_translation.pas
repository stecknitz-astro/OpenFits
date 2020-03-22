unit U_Translation;

{
Date March-01 2020
Author: Frank Szemkus, Von-Parkentin-Str. 2 23919 Berkenthin GERMANY
eMail: kontakt@stecknitz-astronomie.de
Project: OpenFits
Copyright (C) 2020  Frank Szemkus
License: GPL V3

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, ExtCtrls, ComCtrls, StdCtrls, StrUtils,
  Menus, Buttons;

procedure IniText(Form: TForm; sLANG_ID: string);
function TranslateTextTo(sLANG_ID, sText: string): string;
function GermanChars(sText: string): string;

implementation

function GermanChars(sText: string): string;
{2020/03/01 / fs
Replaces AE by Ä, ae by ä, OE by ö a.s.o
}
begin
  Result := sText;
  if(AnsiContainsStr(sText,'uenos')) then exit;
  if(AnsiContainsStr(sText,'srael')) then exit;
  if(AnsiContainsStr(sText,'que')) then exit;
  if(AnsiContainsStr(sText,'uert')) then exit;

  Result := AnsiReplaceStr(Result,'AE','Ä');
  Result := AnsiReplaceStr(Result,'ae','ä');
  Result := AnsiReplaceStr(Result,'OE','Ö');
  Result := AnsiReplaceStr(Result,'oe','ö');
  Result := AnsiReplaceStr(Result,'UE','Ü');
  Result := AnsiReplaceStr(Result,'ue','ü');
end;

function TranslateTextTo(sLANG_ID, sText: string): string;
{2020/03/01 / fs
Identifies a text string and returns a text string in a languaged addressed by sLANG_ID parameter
}
begin
  Result := sText;

  if(sLANG_ID = 'EN') then
  begin
    if(sText = 'Abbrechen') then begin Result := 'Cancel'; exit; end;
    if(sText = 'Abschnitt') then begin Result := 'Section'; exit; end;
    if(sText = 'Alle') then begin Result := 'All'; exit; end;
    if(sText = 'Alle sichtbaren auf Bildgröße') then begin Result := 'Shrink all visible to image'; exit; end;
    if(sText = 'Alle verkleinern') then begin Result := 'Minimize all'; exit; end;
    if(sText = 'Ändern') then begin Result := 'Change'; exit; end;
    if(sText = 'Ändern...') then begin Result := 'Change...'; exit; end;
    if(sText = 'Ansicht') then begin Result := 'View'; exit; end;
    if(sText = 'Auf Bildgröße') then begin Result := 'Shrink to image'; exit; end;
    if(sText = 'Autore(n)') then begin Result := 'Author(s)'; exit; end;
    if(sText = 'Bearbeiten') then begin Result := 'Edit'; exit; end;
    if(sText = 'Beenden') then begin Result := 'Close'; exit; end;
    if(sText = 'Beispiele') then begin Result := 'Examples'; exit; end;
    if(sText = 'Berechne') then begin Result := 'Calculate'; exit; end;
    if(sText = 'Beschreibung') then begin Result := 'Description'; exit; end;
    if(sText = 'Beschriftungen') then begin Result := 'Labels'; exit; end;
    if(sText = 'Bild') then begin Result := 'Picture'; exit; end;
    if(sText = 'Bildbetrachter') then begin Result := 'Pictureviewer'; exit; end;
    if(sText = 'Bild hinzufügen') then begin Result := 'Add picture'; exit; end;
    if(sText = 'Blau') then begin Result := 'Blue'; exit; end;
    if(sText = 'Datei') then begin Result := 'File'; exit; end;
    if(sText = 'Datenbank') then begin Result := 'Database'; exit; end;
    if(sText = 'Datenschutz') then begin Result := 'Privacy Statement'; exit; end;
    if(sText = 'Datum') then begin Result := 'Date'; exit; end;
    if(sText = 'Deutsch') then begin Result := 'German'; exit; end;
    if(sText = 'Diagramm') then begin Result := 'Diagram'; exit; end;
    if(sText = 'Durchmesser') then begin Result := 'Diameter'; exit; end;
    if(sText = 'Eigenschaften') then begin Result := 'Properties'; exit; end;
    if(sText = 'Einblenden') then begin Result := 'Fade in'; exit; end;
    if(sText = 'Eingabemaske leeren') then begin Result := 'Clear Input Mask'; exit; end;
    if(sText = 'Einheit') then begin Result := 'Unit'; exit; end;
    if(sText = 'Englisch') then begin Result := 'English'; exit; end;
    if(sText = 'Entfernen') then begin Result := 'Remove'; exit; end;
    if(sText = 'Farben') then begin Result := 'Color'; exit; end;
    if(sText = 'Fenster') then begin Result := 'Window'; exit; end;
    if(sText = 'Farbe addieren') then begin Result := 'Add color'; exit; end;
    if(sText = 'Farbe multiplizieren') then begin Result := 'Multiply color'; exit; end;
    if(sText = 'Farbbereich unten begrenzen') then begin Result := 'Set value range low limit'; exit; end;
    if(sText = 'Foto') then begin Result := 'Photo'; exit; end;
    if(sText = 'Fotos') then begin Result := 'Pictures'; exit; end;
    if(sText = 'Gerätename') then begin Result := 'Device Name'; exit; end;
    if(sText = 'Grad') then begin Result := 'Degree'; exit; end;
    if(sText = 'Grün') then begin Result := 'Green'; exit; end;
    if(sText = 'Heute') then begin Result := 'Today'; exit; end;
    if(sText = 'Hilfe') then begin Result := 'Help'; exit; end;
    if(sText = 'Hinzufügen') then begin Result := 'Add'; exit; end;
    if(sText = 'Histogramm') then begin Result := 'Histogram'; exit; end;
    if(sText = 'Jetzt') then begin Result := 'Now'; exit; end;
    if(sText = 'Klonen') then begin Result := 'Clone'; exit; end;
    if(sText = 'Löschen') then begin Result := 'Delete'; exit; end;
    if(sText = 'Maske neu') then begin Result := 'Form new'; exit; end;
    if(sText = 'mittel') then begin Result := 'medium'; exit; end;
    if(sText = 'Nebeneinander') then begin Result := 'Tile'; exit; end;
    if(sText = 'Öffnen') then begin Result := 'Open'; exit; end;
    if(sText = 'Optionen') then begin Result := 'Options'; exit; end;
    if(sText = 'Perspektive: Neigungsansicht') then begin Result := 'Perspektive: Inclination view'; exit; end;
    if(sText = 'Pixelgröße') then begin Result := 'Pixel Size'; exit; end;
    if(sText = 'Pixelmathematik') then begin Result := 'Pixel Mathematics'; exit; end;
    if(sText = 'Punkte') then begin Result := 'Points'; exit; end;
    if(sText = 'Rot') then begin Result := 'Red'; exit; end;
    if(sText = 'schwach') then begin Result := 'faint'; exit; end;
    if(sText = 'Sekunden') then begin Result := 'Seconds'; exit; end;
    if(sText = 'Sichern') then begin Result := 'Save'; exit; end;
    if(sText = 'Speichern unter...') then begin Result := 'Save as...'; exit; end;
    if(sText = 'Spektraltyp') then begin Result := 'Spectral Type'; exit; end;
    if(sText = 'Spenden') then begin Result := 'Donate'; exit; end;
    if(sText = 'Sprache') then begin Result := 'Language'; exit; end;
    if(sText = 'Stunden (dezimal)') then begin Result := 'Hours (decimal)'; exit; end;
    if(sText = 'Suche') then begin Result := 'Search'; exit; end;
    if(sText = 'Süd') then begin Result := 'South'; exit; end;
    if(sText = 'Tabellen') then begin Result := 'Spreadsheets'; exit; end;
    if(sText = 'Tag') then begin Result := 'Day'; exit; end;
    if(sText = 'Typ') then begin Result := 'Type'; exit; end;
    if(sText = 'Übereinander') then begin Result := 'Stacked'; exit; end;
    if(sText = 'Überlappend') then begin Result := 'Overlapping'; exit; end;
    if(sText = 'Übernehmen') then begin Result := 'Accept'; exit; end;
    if(sText = 'Übertrage Skalierung auf Bildwerte') then begin Result := 'Transfer scaling into image'; exit; end;
    if(sText = 'Verbergen') then begin Result := 'Hide'; exit; end;
    if(sText = 'Vergrößerungen') then begin Result := 'Magnifications'; exit; end;
    if(sText = 'Visualisierung') then begin Result := 'Visualisation'; exit; end;
    if(sText = 'Voreinstellungen') then begin Result := 'Preferences'; exit; end;
    if(sText = 'Viel') then begin Result := 'High'; exit; end;
    if(sText = 'Wenig') then begin Result := 'Low'; exit; end;
    if(sText = 'Wert addieren') then begin Result := 'Add value'; exit; end;
    if(sText = 'Winkel') then begin Result := 'Angles'; exit; end;
    if(sText = 'Zeige Optionen') then begin Result := 'Show Options'; exit; end;
    if(sText = 'Zeit') then begin Result := 'Time'; exit; end;
    if(sText = 'Zeiteinstellungen') then begin Result := 'Time Settings'; exit; end;
    if(sText = 'Zeitzone') then begin Result := 'Time Zone'; exit; end;
    if(sText = 'Zurücksetzen') then begin Result := 'Reset'; exit; end;
  end;

  if(sLANG_ID = 'DE') then
  begin
    if(sText = 'Accept') then begin Result := 'Übernehmen'; exit; end;
    if(sText = 'Add') then begin Result := 'Hinzufügen'; exit; end;
    if(sText = 'Add color') then begin Result := 'Farbe addieren'; exit; end;
    if(sText = 'Add picture') then begin Result := 'Bild hinzufügen'; exit; end;
    if(sText = 'Add value') then begin Result := 'Wert addieren'; exit; end;
    if(sText = 'All') then begin Result := 'Alle'; exit; end;
    if(sText = 'Angles') then begin Result := 'Winkel'; exit; end;
    if(sText = 'Animations') then begin Result := 'Animationen'; exit; end;
    if(sText = 'Author(s)') then begin Result := 'Autore(n)'; exit; end;
    if(sText = 'Auxiliary Lines') then begin Result := 'Hilfslinien'; exit; end;
    if(sText = 'Blue') then begin Result := 'Blau'; exit; end;
    if(sText = 'Calculate') then begin Result := 'Berechne'; exit; end;
    if(sText = 'Cancel') then begin Result := 'Abbrechen'; exit; end;
    if(sText = 'Change') then begin Result := 'Ändern'; exit; end;
    if(sText = 'Change...') then begin Result := 'Ändern...'; exit; end;
    if(sText = 'Clear Input Mask') then begin Result := 'Eingabemaske leeren'; exit; end;
    if(sText = 'Clear Mask') then begin Result := 'Maske leeren'; exit; end;
    if(sText = 'Close') then begin Result := 'Beenden'; exit; end;
    if(sText = 'Clone') then begin Result := 'Klonen'; exit; end;
    if(sText = 'Color') then begin Result := 'Farben'; exit; end;
    if(sText = 'Database') then begin Result := 'Datenbank'; exit; end;
    if(sText = 'Date') then begin Result := 'Datum'; exit; end;
    if(sText = 'Day') then begin Result := 'Tag'; exit; end;
    if(sText = 'Degree') then begin Result := 'Grad'; exit; end;
    if(sText = 'Delete') then begin Result := 'Löschen'; exit; end;
    if(sText = 'Description') then begin Result := 'Beschreibung'; exit; end;
    if(sText = 'Diagram') then begin Result := 'Diagramm'; exit; end;
    if(sText = 'Donate') then begin Result := 'Spenden'; exit; end;
    if(sText = 'Edit') then begin Result := 'Bearbeiten'; exit; end;
    if(sText = 'English') then begin Result := 'Englisch'; exit; end;
    if(sText = 'Examples') then begin Result := 'Beispiele'; exit; end;
    if(sText = 'Fade in') then begin Result := 'Einblenden'; exit; end;
    if(sText = 'faint') then begin Result := 'schwach'; exit; end;
    if(sText = 'File') then begin Result := 'Datei'; exit; end;
    if(sText = 'Form new') then begin Result := 'Maske neu'; exit; end;
    if(sText = 'German') then begin Result := 'Deutsch'; exit; end;
    if(sText = 'Green') then begin Result := 'Grün'; exit; end;
    if(sText = 'Help') then begin Result := 'Hilfe'; exit; end;
    if(sText = 'Hide') then begin Result := 'Verbergen'; exit; end;
    if(sText = 'High') then begin Result := 'Viel'; exit; end;
    if(sText = 'Highlight') then begin Result := 'Hervorheben'; exit; end;
    if(sText = 'Histogram') then begin Result := 'Histogramm'; exit; end;
    if(sText = 'Inner') then begin Result := 'Inneres'; exit; end;
    if(sText = 'Labels') then begin Result := 'Beschriftungen'; exit; end;
    if(sText = 'Language') then begin Result := 'Sprache'; exit; end;
    if(sText = 'Line Thickness') then begin Result := 'Linienstärke'; exit; end;
    if(sText = 'Location') then begin Result := 'Standort'; exit; end;
    if(sText = 'Low') then begin Result := 'Wenig'; exit; end;
    if(sText = 'Manual (PDF)') then begin Result := 'Dokumentation (PDF)'; exit; end;
    if(sText = 'Maximum Magnitude') then begin Result := 'Maximale Magnitude'; exit; end;
    if(sText = 'Minimize all') then begin Result := 'Alle verkleinern'; exit; end;
    if(sText = 'Multiply color') then begin Result := 'Farbe multiplizieren'; exit; end;
    if(sText = 'Night') then begin Result := 'Nacht'; exit; end;
    if(sText = 'Now') then begin Result := 'Jetzt'; exit; end;
    if(sText = 'Open') then begin Result := 'Öffnen'; exit; end;
    if(sText = 'Options') then begin Result := 'Optionen'; exit; end;
    if(sText = 'Overlapping') then begin Result := 'Überlappend'; exit; end;
    if(sText = 'Photo') then begin Result := 'Foto'; exit; end;
    if(sText = 'Pictureviewer') then begin Result := 'Bildbetrachter'; exit; end;
    if(sText = 'Picture') then begin Result := 'Bild'; exit; end;
    if(sText = 'Pixel Mathematics') then begin Result := 'Pixelmathematik'; exit; end;
    if(sText = 'Pixel Size') then begin Result := 'Pixelgröße'; exit; end;
    if(sText = 'Points') then begin Result := 'Punkte'; exit; end;
    if(sText = 'Preferences') then begin Result := 'Voreinstellungen'; exit; end;
    if(sText = 'Privacy Statement') then begin Result := 'Datenschutz'; exit; end;
    if(sText = 'Properties') then begin Result := 'Eigenschaften'; exit; end;
    if(sText = 'Red') then begin Result := 'Rot'; exit; end;
    if(sText = 'Remove') then begin Result := 'Entfernen'; exit; end;
    if(sText = 'Reset') then begin Result := 'Zurücksetzen'; exit; end;
    if(sText = 'Save') then begin Result := 'Sichern'; exit; end;
    if(sText = 'Save as...') then begin Result := 'Speichern unter...'; exit; end;
    if(sText = 'Search') then begin Result := 'Suche'; exit; end;
    if(sText = 'Seconds') then begin Result := 'Sekunden'; exit; end;
    if(sText = 'Section') then begin Result := 'Abschnitt'; exit; end;
    if(sText = 'Set color range low limit') then begin Result := 'Farbbereich unten begrenzen'; exit; end;
    if(sText = 'Shrink to image') then begin Result := 'Auf Bildgröße'; exit; end;
    if(sText = 'Shrink all visible to image') then begin Result := 'Alle sichtbaren auf Bildgröße'; exit; end;
    if(sText = 'Spreadsheets') then begin Result := 'Tabellen'; exit; end;
    if(sText = 'Stacked') then begin Result := 'Übereinander'; exit; end;
    if(sText = 'strong') then begin Result := 'stark'; exit; end;
    if(sText = 'Time') then begin Result := 'Zeit'; exit; end;
    if(sText = 'Time Settings') then begin Result := 'Zeiteinstellungen'; exit; end;
    if(sText = 'Time Zone') then begin Result := 'Zeitzone'; exit; end;
    if(sText = 'Today') then begin Result := 'Heute'; exit; end;
    if(sText = 'Tile') then begin Result := 'Nebeneinander'; exit; end;
    if(sText = 'Transfer scaling into image') then begin Result := 'Übertrage Skalierung auf Bildwerte'; exit; end;
    if(sText = 'Type') then begin Result := 'Typ'; exit; end;
    if(sText = 'Unit') then begin Result := 'Einheit'; exit; end;
    if(sText = 'Window') then begin Result := 'Fenster'; exit; end;
    if(sText = 'View') then begin Result := 'Ansicht'; exit; end;
    if(sText = 'Visualisation') then begin Result := 'Visualisierung'; exit; end;
  end;

end;

procedure IniText(Form: TForm; sLANG_ID: string);
{2020/03/01 / fs
- Runs on any form component
- Identivies the form component class by naming convention
- Identifies text properties
- Performs the translation function on any detected text property
}
var
  i: Integer;
  sCompName, sText, sTransText: string;
begin
  try

  sText := Form.Caption;
  sTransText := TranslateTextTo(sLANG_ID,sText);
  Form.Caption := sTransText;

  for i:=0 to Form.ComponentCount-1 do
  begin
    sCompName := Uppercase(Trim(Form.Components[i].Name));

    if(LeftStr(sCompName,5) = 'GBX__') then
    begin
      sText := (Form.Components[i] as TGroupBox).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
        begin
          (Form.Components[i] as TGroupBox).Caption := sTransText;
        end;
      end;
      sText := (Form.Components[i] as TGroupBox).Hint;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
        begin
          (Form.Components[i] as TGroupBox).Hint := sTransText;
        end;
      end;
    end;

    if(LeftStr(sCompName,3) = 'P__') then
    begin
      sText := (Form.Components[i] as TPanel).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TPanel).Caption := sTransText;
      end;
      sText := (Form.Components[i] as TPanel).Hint;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TPanel).Hint := sTransText;
      end;
    end;

    if(LeftStr(sCompName,6) = 'MENU__') then
    begin
      sText := (Form.Components[i] as TMenuItem).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TMenuItem).Caption := sTransText;
      end;
    end;

    if(LeftStr(sCompName,7) = 'PMENU__') then
    begin
      if(Uppercase(Form.Components[i].ClassName) <> 'TPOPUPMENU') then
      begin
        sText := (Form.Components[i] as TMenuItem).Caption;
        if(Trim(sText) <> '') then
        begin
          sTransText := TranslateTextTo(sLANG_ID,sText);
          if(Trim(sTransText) <> '') then
            (Form.Components[i] as TMenuItem).Caption := sTransText;
        end;
      end;
    end;

    if(LeftStr(sCompName,3) = 'B__') then
    begin
      sText := (Form.Components[i] as TButton).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TButton).Caption := sTransText;
      end;
      sText := (Form.Components[i] as TButton).Hint;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TButton).Hint := sTransText;
      end;
    end;

    if(LeftStr(sCompName,5) = 'TGB__') then
    begin
      sText := (Form.Components[i] as TToggleBox).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TToggleBox).Caption := sTransText;
      end;
      sText := (Form.Components[i] as TToggleBox).Hint;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TToggleBox).Hint := sTransText;
      end;
    end;

    if(LeftStr(sCompName,4) = 'BT__') then
    begin
      sText := (Form.Components[i] as TBitBtn).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TBitBtn).Caption := sTransText;
      end;
    end;

    if(LeftStr(sCompName,3) = 'L__') then
    begin
      sText := (Form.Components[i] as TLabel).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TLabel).Caption := sTransText;
      end;
      sText := (Form.Components[i] as TLabel).Hint;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TLabel).Hint := sTransText;
      end;
    end;

    if(LeftStr(sCompName,5) = 'CBX__') then
    begin
      sText := (Form.Components[i] as TCheckBox).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TCheckBox).Caption := sTransText;
      end;
    end;

    if(LeftStr(sCompName,4) = 'RB__') then
    begin
      sText := (Form.Components[i] as TRadioButton).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TRadioButton).Caption := sTransText;
      end;
    end;

    if(LeftStr(sCompName,4) = 'TS__') then
    begin
      sText := (Form.Components[i] as TTabSheet).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TTabSheet).Caption := sTransText;
      end;
    end;

    if(LeftStr(sCompName,5) = 'IMG__') then
    begin
      sText := (Form.Components[i] as TImage).Hint;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TImage).Hint := sTransText;
      end;
    end;
  end;
  except
    on e: Exception do
    begin
      ShowMessage('IniText-Err for comp: ' + sCompName + ', Err: ' + e.Message);
    end;
  end;
end;

end.

