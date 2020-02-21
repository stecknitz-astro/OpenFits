unit U_OpenFits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus;

type

  { TF__OPENFITS }

  TF__OPENFITS = class(TForm)
    MENU__FILEOPEN: TMenuItem;
    MENU__FILE: TMenuItem;
    MENU__OPENFITS: TMainMenu;
    ODLG__OPENFITS: TOpenDialog;
    TB__OPENFITS: TToolBar;
    TB__FILEOPEN: TToolButton;
    procedure MENU__FILEOPENClick(Sender: TObject);
  private

  public

  end;

var
  F__OPENFITS: TF__OPENFITS;

implementation

{$R *.lfm}

{ TF__OPENFITS }

procedure TF__OPENFITS.MENU__FILEOPENClick(Sender: TObject);
begin
  if(ODLG__OPENFITS.Execute) and (ODLG__OPENFITS.FileName <> '') then
  begin

  end;
end;

end.

