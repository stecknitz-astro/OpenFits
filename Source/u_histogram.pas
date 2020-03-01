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
  StdCtrls, TAGraph, TASeries,
  Math,
  U_Const;

type

  { TF__HISTOGRAM }

  TF__HISTOGRAM = class(TForm)
    CBX__GREEN: TCheckBox;
    CBX__BLUE: TCheckBox;
    CHART: TChart;
    CHARTLineSeries1: TLineSeries;
    CHARTLineSeries2: TLineSeries;
    CHARTLineSeries3: TLineSeries;
    CHARTLineSeries4: TLineSeries;
    CBX__RED: TCheckBox;
    GBX__COLORS: TGroupBox;
    L__HIST: TLabel;
    L__GAMMA: TLabel;
    P__RED: TPanel;
    PC__CONTROL: TPageControl;
    P__GREEN: TPanel;
    P__BLUE: TPanel;
    P__TOOLS: TPanel;
    P__CONTROL: TPanel;
    TB__GAMMA: TTrackBar;
    TB__HIST: TTrackBar;
    TS__POINTS: TTabSheet;
    TS__HISTOGRAM: TTabSheet;
    TS__GAMMA: TTabSheet;
    procedure FormActivate(Sender: TObject);
    procedure PC__CONTROLChange(Sender: TObject);
    procedure TB__GAMMAKeyPress(Sender: TObject; var Key: char);
    procedure TB__GAMMAMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TB__HISTKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TB__HISTMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure GammaChange();
    procedure HistChange();
  public

  end;

var
  F__HISTOGRAM: TF__HISTOGRAM;

implementation

uses
  U_OpenFits;

{$R *.lfm}

{ TF__HISTOGRAM }

procedure TF__HISTOGRAM.HistChange();
var
  i: Integer;
  rX, rY: Real;
  rContrast: Real; // Contrast values
const
  rcXMax = 10.0;
begin
  rContrast := TB__HIST.Position/10.0;

  L__HIST.Caption := FloatToStrF(rContrast,ffFixed,8,2);

  (CHART.Series[3] as TLineSeries).Clear;
  for i:=1 to ciBit_16-1 do
  begin
    rX := rcXMax*i/ciBit_16;
    rY := ((Pi/2.0) + arctan2(rContrast*(rX - rcXMax/2.0),1))/Pi*100; // https://de.wikipedia.org/wiki/Gammakorrektur
    (CHART.Series[3] as TLineSeries).AddXY(i,rY);
  end;

  F__OPENFITS.ModifyActiveImage(rContrast,ofpHist,CBX__RED.Checked,CBX__GREEN.Checked,CBX__BLUE.Checked);

end;


procedure TF__HISTOGRAM.GammaChange();
var
  i: Integer;
  rX, rY: Real;
  rGamma: Real; // used as argument for y=x(1/rGamma)
begin
  if(TB__GAMMA.Position < 0) then
    rGamma := 1.0/(-TB__GAMMA.Position/10.0 + 1)
  else if(TB__GAMMA.Position = 0) then
    rGamma := 1
  else
    rGamma := TB__GAMMA.Position/10.0 + 1;

  L__GAMMA.Caption := FloatToStrF(rGamma,ffFixed,8,2);

  (CHART.Series[3] as TLineSeries).Clear;
  for i:=1 to ciBit_16-1 do
  begin
    rX := 1.0*i/ciBit_16;
    rY := 100*Power(rX,1.0/rGamma);
    (CHART.Series[3] as TLineSeries).AddXY(i,rY);
  end;

  F__OPENFITS.ModifyActiveImage(rGamma,ofpGamma,CBX__RED.Checked,CBX__GREEN.Checked,CBX__BLUE.Checked);

end;

procedure TF__HISTOGRAM.TB__GAMMAKeyPress(Sender: TObject; var Key: char);
begin
  GammaChange();
end;

procedure TF__HISTOGRAM.TB__GAMMAMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GammaChange();
end;

procedure TF__HISTOGRAM.TB__HISTKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  HistChange();
end;

procedure TF__HISTOGRAM.TB__HISTMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  HistChange();
end;

procedure TF__HISTOGRAM.FormActivate(Sender: TObject);
begin
  if(F__OPENFITS.mbNewLoaded) then
  begin
    PC__CONTROL.ActivePageIndex:=0;
    TB__GAMMA.Position:=0;
    L__GAMMA.Caption := FloatToStrF(1.0,ffFixed,8,2);
    TB__HIST.Position:=5;
  end;
  F__OPENFITS.mbNewLoaded := false;
end;

procedure TF__HISTOGRAM.PC__CONTROLChange(Sender: TObject);
begin
  case PC__CONTROL.ActivePageIndex of
    0: GammaChange();
    1:
    begin
      MessageDlg('OpenFits Histogram','TAB Hist/Contrast: Experimental challenge',mtInformation,[mbOK],0);
      HistChange();
    end;
  end;
end;


end.

