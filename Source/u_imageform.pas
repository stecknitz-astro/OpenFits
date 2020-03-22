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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Types;

type

  { TF__IMG }

  TF__IMG = class(TForm)
    IMG: TImage;
    PB__IMG: TPaintBox;
    SCB__IMG: TScrollBox;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure IMGMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PB__IMGPaint(Sender: TObject);
    procedure SCB__IMGMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure SCB__IMGMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    mrScaleFactor: Real;

    procedure DrawImage();
    procedure ProcessWheel(WheelDelta: Integer);


  public

    marFloatBitmap_R: array of array of Real; // Float buffer array - Intensity of red color pixel
    marFloatBitmap_G: array of array of Real; // Float buffer array - Intensity of green color pixel
    marFloatBitmap_B: array of array of Real; // Float buffer array - Intensity of blue color pixel
    miXDim, miYDim: Integer; // Dimensions of the float buffer array
    miMaxIntensity: Integer; // Maximum intensity value of the float buffer array
    mbModBuffer: Boolean; // Modify Buffer. Strangely an addition argument of ModifyBuffer leads to gray images!!!!!!!

  end;

var
  F__IMG: TF__IMG;

implementation

uses
  U_OpenFits;

{$R *.lfm}

{ TF__IMG }

procedure TF__IMG.DrawImage();
{19.03.2020/fs
Used to zoom into an image controlled by 'mrScaleFactor'
}
var
  ImageRect: TRect;
begin
  ImageRect := Rect(0,0,
    Round(IMG.Picture.Width * mrScaleFactor),
    Round(IMG.Picture.Height * mrScaleFactor));

  PB__IMG.Width := ImageRect.Right;
  PB__IMG.Height := ImageRect.Bottom;
  PB__IMG.Canvas.StretchDraw(ImageRect, IMG.Picture.Bitmap);
end;

procedure TF__IMG.FormActivate(Sender: TObject);
begin
  F__OPENFITS.miActiveImgForm := Tag;
  mrScaleFactor := 1;
end;

procedure TF__IMG.FormClose(Sender: TObject; var CloseAction: TCloseAction);
{12.03.2020/fs
Delete menu entry and un-register
}var
  iMenuIndex: Integer;
begin
  iMenuIndex := F__OPENFITS.MENU__WINDOW.IndexOfCaption(Caption);

  if(iMenuIndex > -1) then
    F__OPENFITS.MENU__WINDOW.Delete(iMenuIndex);

  F__OPENFITS.UnregisterFloatBitmap();
end;

procedure TF__IMG.FormCreate(Sender: TObject);
begin
  mbModBuffer := false;
end;

procedure TF__IMG.IMGMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if(IMG.Visible) then
    ProcessWheel(WheelDelta);
end;

procedure TF__IMG.ProcessWheel(WheelDelta: Integer);
{19.03.2020/fs
Acitities performed when a mouse wheel event is detected :
1. step: Enlarge the image until screen width
2. step: If screen width is exceeded perform zooming
}
var
  bFormSized: Boolean;
const
  ciWidthStep = 20;
begin
  bFormSized := false;

  if(WheelDelta >= 0) then
  begin
    if(Width + ciWidthStep < Screen.Width) then
    begin
      IMG.Visible:=true; SCB__IMG.Visible:=false; PB__IMG.Visible:=false;
      Width := Width + ciWidthStep;
      bFormSized := true;
    end
    else // Zooming!
    begin
      IMG.Visible:=false; SCB__IMG.Visible:=true; PB__IMG.Visible:=true;
      if mrScaleFactor < 50 then
      begin
        mrScaleFactor := mrScaleFactor + 0.1;
        DrawImage();
      end;
    end;
  end
  else
  begin
    if(Width > ciWidthStep) and (mrScaleFactor = 1) then
    begin
      IMG.Visible:=true; SCB__IMG.Visible:=false; PB__IMG.Visible:=false;
      Width := Width - ciWidthStep;
      bFormSized := true;
    end
    else if(mrScaleFactor > 1) then
    begin
      mrScaleFactor := mrScaleFactor - 0.1;
      DrawImage();
    end;

  end;

  if(bFormSized) then
    Height := IMG.Width * IMG.Picture.Height div IMG.Picture.Width;

end;

procedure TF__IMG.PB__IMGPaint(Sender: TObject);
begin
  DrawImage();
end;

procedure TF__IMG.SCB__IMGMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if(SCB__IMG.Visible) then
    ProcessWheel(-1);
end;

procedure TF__IMG.SCB__IMGMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if(SCB__IMG.Visible) then
    ProcessWheel(1);
end;

end.

