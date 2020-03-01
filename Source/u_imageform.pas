unit U_ImageForm;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TF__IMG }

  TF__IMG = class(TForm)
    IMG: TImage;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  F__IMG: TF__IMG;

implementation

uses
  U_OpenFits;

{$R *.lfm}

{ TF__IMG }

procedure TF__IMG.FormActivate(Sender: TObject);
begin
  F__OPENFITS.miActiveImgForm := Tag;
end;

procedure TF__IMG.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  iMenuIndex: Integer;
begin
  iMenuIndex := F__OPENFITS.MENU__WINDOW.IndexOfCaption(Caption);

  if(iMenuIndex > -1) then
    F__OPENFITS.MENU__WINDOW.Delete(iMenuIndex);

  F__OPENFITS.UnregisterFloatBitmap();
end;

end.

