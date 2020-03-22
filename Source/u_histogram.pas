unit u_histogram;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Menus, TAGraph, TASeries,
  Math,
  U_Const;

type

  { TF__HISTOGRAM }

  TF__HISTOGRAM = class(TForm)
    CBX__GREEN: TCheckBox;
    CBX__BLUE: TCheckBox;
    CBX__RED: TCheckBox;
    CHART: TChart;
    CHARTConstantLine1: TConstantLine;
    CHARTConstantLine2: TConstantLine;
    CHARTLineSeries1: TLineSeries;
    CHARTLineSeries2: TLineSeries;
    CHARTLineSeries3: TLineSeries;
    CHARTLineSeries4: TLineSeries;
    GBX__COLORS: TGroupBox;
    L__HIST: TLabel;
    L__GAMMA: TLabel;
    MENU__TRANSFER_SCALE: TMenuItem;
    PMENU_HISTO: TPopupMenu;
    P__LINE_SEL_L: TPanel;
    P__CHART_LR_CONTROL: TPanel;
    P__CHART_CONTROL: TPanel;
    P__LINE_SEL_R: TPanel;
    P__RED: TPanel;
    PC__CONTROL: TPageControl;
    P__GREEN: TPanel;
    P__BLUE: TPanel;
    P__TOOLS: TPanel;
    P__CONTROL: TPanel;
    TB__GAMMA: TTrackBar;
    TB__HIST: TTrackBar;
    TB__LINE: TTrackBar;
    TS__POINTS: TTabSheet;
    TS__HISTOGRAM: TTabSheet;
    TS__GAMMA: TTabSheet;
    procedure FormActivate(Sender: TObject);
    procedure MENU__TRANSFER_SCALEClick(Sender: TObject);
    procedure PC__CONTROLChange(Sender: TObject);
    procedure P__LINE_SEL_LClick(Sender: TObject);
    procedure P__LINE_SEL_RClick(Sender: TObject);
    procedure TB__GAMMAKeyPress(Sender: TObject; var Key: char);
    procedure TB__GAMMAMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TB__HISTKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TB__HISTMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TB__LINEChange(Sender: TObject);
  private
    mbLeftMode: Boolean;

    procedure GammaChange(bModImage: Boolean);
    procedure HistChange(bModImage: Boolean);
    procedure IniHist();

  public

  end;

var
  F__HISTOGRAM: TF__HISTOGRAM;

implementation

uses
  U_OpenFits, U_ImageForm;

{$R *.lfm}

{ TF__HISTOGRAM }

procedure TF__HISTOGRAM.IniHist();
{2020/03/01 / fs
Things to do when the histogram form is opened.
Distinguished between:
- a new image is evaluated (mNewLoaded = TRUE) to set on default values
- an imgae is reopened (mbNewLoaded = FALSE)
}
begin
  if(F__OPENFITS.mbNewLoaded) then
  begin
    PC__CONTROL.ActivePageIndex:=0;
    TB__GAMMA.Position:=0;
    L__GAMMA.Caption := FloatToStrF(1.0,ffFixed,8,2);
    TB__HIST.Position:=5;

    (CHART.Series[4] as TConstantLine).Position:=1;
    (CHART.Series[5] as TConstantLine).Position:=(ciMaxBits-1);

    mbLeftMode := true;
    P__LINE_SEL_L.Caption:='L';
    P__LINE_SEL_R.Caption:='';
    TB__LINE.Position := Trunc(((CHART.Series[4] as TConstantLine).Position / ciMaxBits)*TB__LINE.Max);
    GammaChange(false);
    //HistChange(false);
  end;
  F__OPENFITS.mbNewLoaded := false;
end;

procedure TF__HISTOGRAM.HistChange(bModImage: Boolean);
{2020/03/01 / fs
Controls the shape of the arctan-contrast functions by the position of the trackbutton TB__HIST.
Modifies the addressed image by the modified contrast function
}
var
  i: Integer;
  rX, rY: Real;
  rContrast: Real;
  fcContrast, fcContrast2: TFloatColor; // Contrast values
const
  rcXMax = 10.0;
begin
  rContrast := TB__HIST.Position/10.0;

  L__HIST.Caption := FloatToStrF(rContrast,ffFixed,8,2);

  (CHART.Series[3] as TLineSeries).Clear;
  for i:=0 to ciMaxBits-1 do
  begin
    rX := rcXMax*i/(ciMaxBits-1);
    rY := ((Pi/2.0) + arctan2(rContrast*(rX - rcXMax/2.0),1))/Pi*100; // https://de.wikipedia.org/wiki/Gammakorrektur
    (CHART.Series[3] as TLineSeries).AddXY(i,rY);
  end;

  fcContrast2.rRed:=0;
  fcContrast2.rGreen:=0;
  fcContrast2.rBlue:=0;

  fcContrast.rRed:=0;
  fcContrast.rGreen:=0;
  fcContrast.rBlue:=0;

  if(CBX__RED.Checked) then
    fcContrast.rRed:=rContrast;

  if(CBX__GREEN.Checked) then
    fcContrast.rGreen:=rContrast;

  if(CBX__BLUE.Checked) then
    fcContrast.rBlue:=rContrast;

  if(bModImage) then
    F__OPENFITS.ModifyActiveImage(fcContrast,fcContrast2,ofpHist);

end;


procedure TF__HISTOGRAM.GammaChange(bModImage: Boolean);
{2020/03/01 / fs
Controls the shape of the gamma-function by the position of the trackbutton TB__GAMMA.
Modifies the addressed image by the modified gamma function
}
var
  i: Integer;
  rX, rY: Real;
  rGamma: Real; // used as argument for y=x(1/rGamma)
  fcGamma, fcArg2: TFloatColor;
begin
  if(TB__GAMMA.Position < 0) then
    rGamma := 1.0/(-TB__GAMMA.Position/10.0 + 1)
  else if(TB__GAMMA.Position = 0) then
    rGamma := 1
  else
    rGamma := TB__GAMMA.Position/10.0 + 1;

  //L__GAMMA.Caption := FloatToStrF(rGamma,ffFixed,8,2);

  (CHART.Series[3] as TLineSeries).Clear;
  for i:=0 to ciMaxBits-1 do
  begin
    rX := 1.0*i/(ciMaxBits-1);
    rY := 100*Power(rX,1.0/rGamma);
    (CHART.Series[3] as TLineSeries).AddXY(i,rY);
  end;

  fcArg2.rRed:=0;
  fcArg2.rGreen:=0;
  fcArg2.rBlue:=0;

  if(CBX__RED.Checked) then
    fcGamma.rRed:=rGamma;

  if(CBX__GREEN.Checked) then
    fcGamma.rGreen:=rGamma;

  if(CBX__BLUE.Checked) then
    fcGamma.rBlue:=rGamma;

  if(bModImage) then
    F__OPENFITS.ModifyActiveImage(fcGamma,fcArg2,ofpGamma);

end;

procedure TF__HISTOGRAM.TB__GAMMAKeyPress(Sender: TObject; var Key: char);
{2020/03/01 / fs
Things to do when a key is pressed while using the trackbutton TB__GAMMA:
Calling GammaChange function.
}
begin
  GammaChange(true);
end;

procedure TF__HISTOGRAM.TB__GAMMAMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{2020/03/01 / fs
Things to do when a mouseup-event is recognized while using the trackbutton TB__GAMMA:
Calling GammaChange function.
}
begin
  GammaChange(true);
end;

procedure TF__HISTOGRAM.TB__HISTKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{2020/03/01 / fs
Things to do when a KeyUp is recognized while using the trackbutton TB__HIST:
Calling HistChange function.
}
begin
  HistChange(true);
end;

procedure TF__HISTOGRAM.TB__HISTMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{2020/03/01 / fs
Things to do when a MouseUp is recognized while using the trackbutton TB__HIST:
Calling HistChange function.
}
begin
  HistChange(true);
end;

procedure TF__HISTOGRAM.TB__LINEChange(Sender: TObject);
{2020/03/01 / fs
Things to do when the trackbutton TB__LINE is changed to re-calculate the
intervals (<L>ow/<H>igh) of the histogram
}
var
  fcArg, fcArg2: TFloatColor;
begin
  if(mbLeftMode) then
    (CHART.Series[4] as TConstantLine).Position := (ciMaxBits div TB__LINE.Max) * TB__LINE.Position
  else
    (CHART.Series[5] as TConstantLine).Position := (ciMaxBits div TB__LINE.Max) * TB__LINE.Position;

  if(CBX__RED.Checked) then
    fcArg.rRed:=(CHART.Series[4] as TConstantLine).Position;

  if(CBX__GREEN.Checked) then
    fcArg.rGreen:=(CHART.Series[4] as TConstantLine).Position;

  if(CBX__BLUE.Checked) then
    fcArg.rBlue:=(CHART.Series[4] as TConstantLine).Position;


  if(CBX__RED.Checked) then
    fcArg2.rRed:=(CHART.Series[5] as TConstantLine).Position;

  if(CBX__GREEN.Checked) then
    fcArg2.rGreen:=(CHART.Series[5] as TConstantLine).Position;

  if(CBX__BLUE.Checked) then
    fcArg2.rBlue:=(CHART.Series[5] as TConstantLine).Position;

  F__OPENFITS.ModifyActiveImage(
    fcArg,
    fcArg2,
    ofpCutLowHigh);

end;

procedure TF__HISTOGRAM.FormActivate(Sender: TObject);
begin
  IniHist();
end;

procedure TF__HISTOGRAM.MENU__TRANSFER_SCALEClick(Sender: TObject);
var
  iNewIndex: Integer;
  F__IMG: TF__IMG;
begin
  iNewIndex := -1;
  F__IMG := F__OPENFITS.CloneImgWindow(F__OPENFITS.miActiveImgForm,iNewIndex);
  if(F__IMG <> nil) and (iNewIndex > -1) then
  begin
    F__OPENFITS.mbNewLoaded := true;
    IniHist();
    F__IMG.Show;
    F__OPENFITS.miActiveImgForm:=iNewIndex;
  end;
end;

procedure TF__HISTOGRAM.PC__CONTROLChange(Sender: TObject);
{2020/03/01 / fs
Things to do when the PageControl tab is changed:
- Gamma function is selected
- Hist function (for contrast) is selected
}
begin
  case PC__CONTROL.ActivePageIndex of
    0: GammaChange(true);
    1:
    begin
      MessageDlg('OpenFits Histogram','TAB Hist/Contrast: Experimental challenge',mtInformation,[mbOK],0);
      HistChange(true);
    end;
  end;
end;

procedure TF__HISTOGRAM.P__LINE_SEL_LClick(Sender: TObject);
{2020/03/01 / fs
Things to do when L-Panelbutton is clicked to activate the LOW range calculation setting
}
begin
  mbLeftMode := true;
  P__LINE_SEL_L.Caption:='L';
  P__LINE_SEL_R.Caption:='';
  TB__LINE.Position := Trunc(((CHART.Series[4] as TConstantLine).Position / ciMaxBits)*TB__LINE.Max) + 1;
end;

procedure TF__HISTOGRAM.P__LINE_SEL_RClick(Sender: TObject);
{2020/03/01 / fs
Things to do when H-Panelbutton is clicked to activate the HIGH range calculation setting
}
begin
  mbLeftMode := false;
  P__LINE_SEL_L.Caption:='';
  P__LINE_SEL_R.Caption:='H';
  TB__LINE.Position := Trunc(((CHART.Series[5] as TConstantLine).Position / ciMaxBits)*TB__LINE.Max) + 1;
end;


end.

