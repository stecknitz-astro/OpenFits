unit u_info;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  U_Const;

type

  { TF__INFO }

  TF__INFO = class(TForm)
    IMG__LOGO: TImage;
    L__EMAIL_TITLE: TLabel;
    L__EMAIL: TLabel;
    L__VERSION_TITLE: TLabel;
    L__VERSION: TLabel;
    L__AUTHOR_TITLE: TLabel;
    L__AUTHOR: TLabel;
    P__APP_NAME: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  F__INFO: TF__INFO;

implementation

{$R *.lfm}

{ TF__INFO }

procedure TF__INFO.FormCreate(Sender: TObject);
begin
  P__APP_NAME.Caption:=csAppName;
  L__VERSION.Caption := csVersionMain + '.' + csVersionSub + '.' + csVersionSubSub;
  L__AUTHOR.Caption := csAuthor;
  L__EMAIL.Caption := csEmail;
end;

end.

