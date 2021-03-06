unit U_OpenFits;

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

{
ABOUT OPENFITS

Unfortunately the author of the famous FITSWORK astroimage manipulation program (Jens Dierks)
has stopped the development for some years.

In the meantime the program is used by numberous astrophotography enthusiasts to develop their astroimages.

This project - OpenFits - is an approach the establish a free astroimage manipulation programm which can be
modified and compiled by anybody. The functionality, design and work-flow will be strongly influenced by FITSWORK.

The original FITSWORK was compiled whith DELPHI. Similar, the OpenFits approach sources can be compiled
with the LAZARUS IDE, which is  a freely available DELPHI variant.
You can download the LAZARUS IDE here: www.lazarus-ide.org
Furthermore, the LAZARUS IDE enables parallel developent for MS Windows and e.g. MACOS
by usage of the same OpenFits source files.

Any FREEPASCAL developer is envited to contribute to this project!

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, ExtDlgs, DeLaFitsClasses, DeLaFitsCommon, DeLaFitsDataRead,
  DeLaFitsDataWrite, DeLaFitsGraphics, DeLaFitsMath, DeLaFitsOrderByte,
  DeLaFitsPalettes, DeLaFitsString,
  Math,
  U_Const, U_ImageForm, U_Histogram, U_Info, U_Translation,
  IntfGraphics, LCLType, StdCtrls, TAGraph, TASeries, FPimage;

const
  ciWindowPosOffset = 40; // Used for opended windows under the main control window of OpenFits

type

  { TF__OPENFITS }

  TF__OPENFITS = class(TForm)
    CDLG: TColorDialog;
    IMGL__LIB: TImageList;
    IMGL__MENU: TImageList;
    L__TEST: TLabel;
    MenuItem1: TMenuItem;
    MENU__EDIT_LIMITLOW: TMenuItem;
    MENU__EDIT_DIVIDE: TMenuItem;
    MENU__EDIT_MULT: TMenuItem;
    MENU__EDIT_MINUS: TMenuItem;
    MENU__EDIT_ADD: TMenuItem;
    MENU__PIXMATH: TMenuItem;
    MENU__CLONE: TMenuItem;
    MENU__EDIT: TMenuItem;
    MENU__LANG_EN: TMenuItem;
    MENU__LANG_DE: TMenuItem;
    MENU__INFO: TMenuItem;
    MENU__HELP: TMenuItem;
    MENU__HISTOGRAM: TMenuItem;
    MenuItem3: TMenuItem;
    MENU__MINIMIZE_ALL: TMenuItem;
    MENU__ADAPT_SIZE_ALL: TMenuItem;
    MENU__ADAPT_SIZE: TMenuItem;
    MENU__WINDOW_STACKED: TMenuItem;
    MENU__WINDOW_SIDEBYSIDE: TMenuItem;
    MENU__WINDOW_OVERLAP: TMenuItem;
    MENU__SAVE: TMenuItem;
    MENU__CLOSE: TMenuItem;
    MENU__FILEOPEN: TMenuItem;
    MENU__FILE: TMenuItem;
    MMENU__OPENFITS: TMainMenu;
    MENU__WINDOW: TMenuItem;
    ODLG__OPENFITS: TOpenDialog;
    SDLG__IMG: TSavePictureDialog;
    TB__OPENFITS: TToolBar;
    TB__FILEOPEN: TToolButton;
    TB__FILESAVE: TToolButton;
    ToolButton1: TToolButton;
    TB__HISTOGRAM: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MENU__ADAPT_SIZEClick(Sender: TObject);
    procedure MENU__ADAPT_SIZE_ALLClick(Sender: TObject);
    procedure MENU__CLONEClick(Sender: TObject);
    procedure MENU__CLOSEClick(Sender: TObject);
    procedure MENU__EDIT_ADDClick(Sender: TObject);
    procedure MENU__EDIT_DIVIDEClick(Sender: TObject);
    procedure MENU__EDIT_LIMITLOWClick(Sender: TObject);
    procedure MENU__EDIT_MINUSClick(Sender: TObject);
    procedure MENU__EDIT_MULTClick(Sender: TObject);
    procedure MENU__FILEOPENClick(Sender: TObject);
    procedure MENU__HISTOGRAMClick(Sender: TObject);
    procedure MENU__INFOClick(Sender: TObject);
    procedure MENU__LANG_DEClick(Sender: TObject);
    procedure MENU__LANG_ENClick(Sender: TObject);
    procedure MENU__MINIMIZE_ALLClick(Sender: TObject);
    procedure MENU__SAVEClick(Sender: TObject);
    procedure MENU__WINDOW_OVERLAPClick(Sender: TObject);
    procedure MENU__WINDOW_SIDEBYSIDEClick(Sender: TObject);
    procedure MENU__WINDOW_STACKEDClick(Sender: TObject);
  private
    // Special Delafits objects

    FFit: TFitsFileBitmap;
    FRgn: TRgn;

    // OpenFits objects

    mslImgLst: TStringList; // Stringlist buffer containing the opened image windows
    mSrcIntfImg: TLazIntfImage; // Image modification memory

    procedure SaveImg();
    procedure OpenFitsMenuClick(Sender: TObject);
    procedure GetHistogramData(ABitMap: Graphics.TBitmap);
    procedure RegisterFloatBitmap(ABitmap: Graphics.TBitmap; iIndex: Integer);
    function GetGammaVal(rIntensity: Real; rGamma: Real): Word;
    function GetHistContrastVal(rIntensity: Real; rHist: Real): Word;
    function GetCutLowHighVal(rIntensity: Real; rArgL, rArgH: Real): Word;
    function GetBasicMathModVal(rIntensity: Real; rArg: Real; OpenFitsPixFunction: TOpenFitsPixFunction): Word;
    procedure ExecBasicImgCalc(OpenFitsPixFunction: TOpenFitsPixFunction);
    procedure GetImgWindowPos(iIndex: Integer; var iLeft: Integer; var iTop: Integer);

  public
    miActiveImgForm: Integer; // Index of the activated image window
    msLANG_ID: string; // Current language ID: DE: German, EN - English
    mbNewLoaded: Boolean; // Set to TRUE only, if a new image windos is opened. Used for special initializing activities

    procedure ModifyActiveImage(fcArg, fcArg2: TFloatColor; OpenFitsPixFunction: TOpenFitsPixFunction);
    procedure UnregisterFloatBitmap();
    function ColorToFloatColor(clColor: TColor): TFloatColor;
    function NewImgWindow(sCaption: string; var iIndex: Integer): TF__IMG;
    function CloneImgWindow(iSourceIndex: Integer; var iNewIndex: Integer): TF__IMG;

  end;

var
  F__OPENFITS: TF__OPENFITS;

type
  TRGBTripleArray = array[0..32767] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;

implementation

{$R *.lfm}

{ TF__OPENFITS }

function TF__OPENFITS.CloneImgWindow(iSourceIndex: Integer; var iNewIndex: Integer): TF__IMG;
{2020/03/20 / fs
Clones an image window, identified by index iSource index.
Returns the index of the cloned window
}
var
  SrcIntfImg: TLazIntfImage;
  sCaption: string;
begin
  Result := nil;
  iNewIndex := -1;

  if(iSourceIndex < 0) or (mslImgLst.Count = 0) then
    exit;

  sCaption := (mslImgLst.Objects[miActiveImgForm] as TF__IMG).Caption + '_Clone';

  SrcIntfImg:=TLazIntfImage.Create(0,0);
  SrcIntfImg.LoadFromBitmap(
    (mslImgLst.Objects[iSourceIndex] as TF__IMG).IMG.Picture.Bitmap.Handle,
    (mslImgLst.Objects[iSourceIndex] as TF__IMG).IMG.Picture.Bitmap.MaskHandle);

  Result := NewImgWindow(sCaption,iNewIndex);
  Result.IMG.Picture.Bitmap.LoadFromIntfImage(SrcIntfImg);

  RegisterFloatBitmap((mslImgLst.Objects[iNewIndex] as TF__IMG).IMG.Picture.Bitmap,iNewIndex);

  SrcIntfImg.Free;

end;

function TF__OPENFITS.NewImgWindow(sCaption: string; var iIndex: Integer): TF__IMG;
{2020/03/17 / fs
Creates and registers a new client image window with an empty image
}
var
  iLeft, iTop: Integer;
begin
  iLeft := 0; iTop := 0;

  Result := TF__IMG.Create(F__OPENFITS);
  mslImgLst.AddObject('F__IMG_' + IntToStr(mslImgLst.Count+1), Result);
  iIndex := mslImgLst.Count-1;
  Result.Name:='F__IMG_' + IntToStr(mslImgLst.Count+1);
  Result.Tag := iIndex;
  Result.Caption:=sCaption;

  GetImgWindowPos(iIndex, iLeft, iTop);

  Result.Left:=iLeft;
  Result.Top:=iTop;
end;

function TF__OPENFITS.ColorToFloatColor(clColor: TColor): TFloatColor;
var
  iColor: LongInt;
begin
  iColor := ColorToRGB(clColor);

  // Set & stretch values
  Result.rRed := Red(iColor); Result.rRed := Result.rRed * Result.rRed;
  Result.rGreen := Green(iColor); Result.rGreen := Result.rGreen * Result.rGreen;
  Result.rBlue := Blue(iColor); Result.rBlue := Result.rBlue * Result.rBlue;

end;

procedure TF__OPENFITS.ExecBasicImgCalc(OpenFitsPixFunction: TOpenFitsPixFunction);
{2020/03/05 / fs
Request and execution call for a basic image manipulation operation
}
var
  fcArg1, fcArg2: TFloatColor;
  sTitle, sQuery: string;
begin
  if(msLANG_ID = 'DE') then
    sTitle := 'Eingabe'
  else
    sTitle := 'Input';

  case OpenFitsPixFunction of
  ofpAdd:
    begin
      if(msLANG_ID = 'DE') then
        sQuery := 'Zu addierender Wert'
      else
        sQuery := 'Adding value';
    end;

  ofpMinus:
    begin
      if(msLANG_ID = 'DE') then
        sQuery := 'Zu subtrahierender Wert'
      else
        sQuery := 'Subtracting value';
    end;

  ofpMult:
    begin
      if(msLANG_ID = 'DE') then
        sQuery := 'Zu multiplizierender Wert > 0'
      else
        sQuery := 'Multiplying value > 0';
    end;

  ofpDiv:
    begin
      if(msLANG_ID = 'DE') then
        sQuery := 'Zu dividierender Wert <> 0'
      else
        sQuery := 'Dividing value <> 0';

    end;

  ofpCutLow:
    begin
      OpenFitsPixFunction := ofpCutLowHigh;

      if(msLANG_ID = 'DE') then
        sQuery := 'Unterer Wert'
      else
        sQuery := 'Lower value';
    end;

  ofpCutHigh:
    begin
      OpenFitsPixFunction := ofpCutLowHigh;

      if(msLANG_ID = 'DE') then
        sQuery := 'Oberer Wert'
      else
        sQuery := 'Upper value';

    end;
  end; // case

  CDLG.Title := sTitle + ' ' + sQuery;

  if(CDLG.Execute) and (CDLG.Color >= 0) then
  begin
    if(OpenFitsPixFunction = ofpCutHigh) then
      fcArg2 := ColorToFloatColor(CDLG.Color)
    else
      fcArg1 := ColorToFloatColor(CDLG.Color);

    ModifyActiveImage(fcArg1,fcArg2,OpenFitsPixFunction);

  end;

end;

function TF__OPENFITS.GetBasicMathModVal(rIntensity: Real; rArg: Real; OpenFitsPixFunction: TOpenFitsPixFunction): Word;
{2020/03/05 / fs
Performs basic mathematical operations on an intensity value (eg. add, suptract, mulitply, divide, ...
}
begin
  case OpenFitsPixFunction of
  ofpAdd:
    if(Trunc(rIntensity + rArg)  < ciMaxBits) then
      Result := Trunc(rIntensity + rArg)
    else
      Result := ciMaxBits-1;

  ofpMinus:
    if(rIntensity - rArg  >= 0) then
      Result := Trunc(rIntensity - rArg)
    else
      Result := 0;

  ofpMult:
    if(rArg >= 0) then
    begin
      if(rIntensity * rArg  < ciMaxBits) then
        Result := Trunc(rIntensity * rArg)
      else
        Result := ciMaxBits-1;

    end;
  ofpDiv:
    if(rArg > 0) then
    begin
      if(rIntensity / rArg  < ciMaxBits) then
        Result := Trunc(rIntensity / rArg)
      else
        Result := ciMaxBits-1;

    end;
  else
    Result := 0;

  end; // case
end;

function TF__OPENFITS.GetCutLowHighVal(rIntensity: Real; rArgL, rArgH: Real): Word;
{2020/03/01 / fs
Setting values to 0 if rIntensity is lower equal rArgL,
setting values to miMaxIntensity if higher equal rArgH
Used in histogram boundary cutting
}
begin
  if(rIntensity <= rArgL) then
    Result := 0
  else if(rIntensity >= rArgH) then
    Result := (mslImgLst.Objects[miActiveImgForm] as TF__IMG).miMaxIntensity//ciMaxBits
  else
    Result := Trunc(rIntensity);

end;

function TF__OPENFITS.GetHistContrastVal(rIntensity: Real; rHist: Real): Word;
{2020/03/01 / fs
Evaluates a parametrized arctan value for contrast function in histogram
}
var
  rX, rY: Real;
const
  rcXMax = 10.0;
begin
  rX := rIntensity*rcXMax/(ciMaxBits-1); // [0..10]!
  rY := (0.5*Pi + arctan2((rX - rcXMax/2)*rHist,1))/Pi *(ciMaxBits-1);

  Result := Trunc(rY);
end;


function TF__OPENFITS.GetGammaVal(rIntensity: Real; rGamma: Real): Word;
{2020/02/23 / fs
Evaluates the gamma power function for histogram modification
}
var
  rX, rY: Real;
begin
  rX := rIntensity/(ciMaxBits-1); // [0..1]!
  rY := Power(rX,1.0/rGamma) *(ciMaxBits-1);

  Result := Trunc(rY);
end;

procedure TF__OPENFITS.UnregisterFloatBitmap();
{2020/02/23 / fs
Clears the RGB-Float buffer array and sets the correspondig diminesions to zero
}
begin
  SetLength((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_R, 0, 0);
  SetLength((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_G, 0, 0);
  SetLength((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_B, 0, 0);

  (mslImgLst.Objects[miActiveImgForm] as TF__IMG).miXDim := 0;
  (mslImgLst.Objects[miActiveImgForm] as TF__IMG).miYDim := 0;
end;

procedure TF__OPENFITS.ModifyActiveImage(fcArg,fcArg2: TFloatColor; OpenFitsPixFunction: TOpenFitsPixFunction);
{2020/03/1 / fs
General image modification function. Working on the RGB-float buffer array. Several modification functions
can be addressed.
}
var
  TempIntfImg: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
  pngbmp: TPortableNetworkGraphic;
  px, py: Integer;
  CurColor: TFPColor;
  MemoryStream: TMemoryStream;
  ABitMap: Graphics.TBitmap;
  iXDim, iYDim: Integer;
begin

  if(miActiveImgForm < 0) or (miActiveImgForm >= mslImgLst.Count) then
    exit;

  //L__TEST.Caption:=FloatToStrF(rArg,ffFixed,8,2);

  // Load active picture indexed by 'miActiveImgForm'
  ABitMap := (mslImgLst.Objects[miActiveImgForm] as TF__IMG).IMG.Picture.Bitmap;

  if(ABitMap = nil) then
    exit;

  iXDim := (mslImgLst.Objects[miActiveImgForm] as TF__IMG).miXDim;
  iYDim := (mslImgLst.Objects[miActiveImgForm] as TF__IMG).miYDim;

  if(iXDim = 0) or (iYDim = 0) then
    exit;

  pngbmp := TPortableNetworkGraphic.Create;
  MemoryStream := TMemoryStream.Create;

  TempIntfImg:=TLazIntfImage.Create(0,0);
  TempIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);

  try
    Screen.Cursor:=crHourGlass;

    for py:=0 to iYDim-1 do begin
      for px:=0 to iXDim-1 do begin

        case OpenFitsPixFunction of
          ofpGamma:
          begin
            if(fcArg.rRed >= 0) then
              CurColor.Red:=GetGammaVal((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_R[px,py],fcArg.rRed);
            if(fcArg.rGreen >= 0) then
              CurColor.Green:=GetGammaVal((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_G[px,py],fcArg.rGreen);
            if(fcArg.rBlue >= 0) then
              CurColor.Blue:=GetGammaVal((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_B[px,py],fcArg.rBlue);
          end;
          ofpHist:
          begin
            if(fcArg.rRed >= 0) then
              CurColor.Red:=GetHistContrastVal((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_R[px,py],fcArg.rRed);
            if(fcArg.rGreen >= 0) then
              CurColor.Green:=GetHistContrastVal((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_G[px,py],fcArg.rGreen);
            if(fcArg.rBlue >= 0) then
              CurColor.Blue:=GetHistContrastVal((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_B[px,py],fcArg.rBlue);
          end;
          ofpCutLowHigh:
          begin
            if(fcArg.rRed >= 0) then
              CurColor.Red:=GetCutLowHighVal((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_R[px,py],fcArg.rRed,fcArg2.rRed);
            if(fcArg.rGreen >= 0) then
              CurColor.Green:=GetCutLowHighVal((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_G[px,py],fcArg.rGreen,fcArg2.rGreen);
            if(fcArg.rBlue >= 0) then
              CurColor.Blue:=GetCutLowHighVal((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_B[px,py],fcArg.rBlue,fcArg2.rBlue);
          end;
          ofpAdd, ofpMinus, ofpMult, ofpDiv:
          begin
            if(fcArg.rRed >= 0) then
              CurColor.Red:=GetBasicMathModVal((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_R[px,py],fcArg.rRed,OpenFitsPixFunction);
            if(fcArg.rGreen >= 0) then
              CurColor.Green:=GetBasicMathModVal((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_G[px,py],fcArg.rGreen,OpenFitsPixFunction);
            if(fcArg.rBlue >= 0) then
              CurColor.Blue:=GetBasicMathModVal((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_B[px,py],fcArg.rBlue,OpenFitsPixFunction);
          end;
          else
          begin
            CurColor.red:=Trunc((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_R[px,py]);
            CurColor.green:=Trunc((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_G[px,py]);
            CurColor.blue:=Trunc((mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_B[px,py]);
          end;
        end; // case

        if((mslImgLst.Objects[miActiveImgForm] as TF__IMG).mbModBuffer) then
        begin
          (mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_R[px,py] := CurColor.red;
          (mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_G[px,py] := CurColor.green;
          (mslImgLst.Objects[miActiveImgForm] as TF__IMG).marFloatBitmap_B[px,py] := CurColor.blue;
        end;

        TempIntfImg.Colors[px,py]:=CurColor;
      end; // px
    end; // py

    TempIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle,false);

    pngbmp.BitmapHandle:=ImgHandle;
    pngbmp.MaskHandle:=ImgMaskHandle;
    pngbmp.SaveToStream(MemoryStream);

    MemoryStream.Position:=0;

    (mslImgLst.Objects[miActiveImgForm] as TF__IMG).IMG.Picture.LoadFromStream(MemoryStream);
  finally
    Screen.Cursor:=crDefault;

    TempIntfImg.Free;
    pngbmp.Free;
    MemoryStream.Free;
  end;

end;

procedure TF__OPENFITS.RegisterFloatBitmap(ABitmap: Graphics.TBitmap; iIndex: Integer);
{2020/02/23 / fs
Registers a selected imaga and writes the image values into
the RGB-Float buffer array
}
var
   SrcIntfImg: TLazIntfImage;
   px, py: Integer;
   CurColor: TFPColor;
begin
  mbNewLoaded := true; // Used to initialize working dialogs like Histogram, ...

  SrcIntfImg:=TLazIntfImage.Create(0,0);
  SrcIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);

  SetLength((mslImgLst.Objects[iIndex] as TF__IMG).marFloatBitmap_R, SrcIntfImg.Width, SrcIntfImg.Height);
  SetLength((mslImgLst.Objects[iIndex] as TF__IMG).marFloatBitmap_G, SrcIntfImg.Width, SrcIntfImg.Height);
  SetLength((mslImgLst.Objects[iIndex] as TF__IMG).marFloatBitmap_B, SrcIntfImg.Width, SrcIntfImg.Height);

  (mslImgLst.Objects[iIndex] as TF__IMG).miXDim := SrcIntfImg.Width;
  (mslImgLst.Objects[iIndex] as TF__IMG).miYDim := SrcIntfImg.Height;
  (mslImgLst.Objects[iIndex] as TF__IMG).miMaxIntensity := 0;

  for py:=0 to SrcIntfImg.Height-1 do
  begin
    for px:=0 to SrcIntfImg.Width-1 do
    begin
      CurColor:=SrcIntfImg.Colors[px,py];

      (mslImgLst.Objects[iIndex] as TF__IMG).marFloatBitmap_R[px,py] := CurColor.red;
      (mslImgLst.Objects[iIndex] as TF__IMG).marFloatBitmap_G[px,py] := CurColor.green;
      (mslImgLst.Objects[iIndex] as TF__IMG).marFloatBitmap_B[px,py] := CurColor.blue;

      if(CurColor.red > (mslImgLst.Objects[iIndex] as TF__IMG).miMaxIntensity) then
        (mslImgLst.Objects[iIndex] as TF__IMG).miMaxIntensity := CurColor.red;
      if(CurColor.green> (mslImgLst.Objects[iIndex] as TF__IMG).miMaxIntensity) then
        (mslImgLst.Objects[iIndex] as TF__IMG).miMaxIntensity := CurColor.green;
      if(CurColor.blue > (mslImgLst.Objects[iIndex] as TF__IMG).miMaxIntensity) then
        (mslImgLst.Objects[iIndex] as TF__IMG).miMaxIntensity := CurColor.blue;

    end;
  end;

  SrcIntfImg.Free;

end;

procedure TF__OPENFITS.GetHistogramData(ABitMap: Graphics.TBitmap);
{2020/02/23 / fs
Evaluates the current Bitmap color counts and displays the resulting histogram.
In dependence of the active tab a corresponding modifiaction curve (e.g. gamma or contrast)
is displayed
}
var
   SrcIntfImg: TLazIntfImage;
   i,px, py: Integer;
   CurColor: TFPColor;
   iaHistColorRed: array[0..ciMaxBits-1] of Integer;
   iaHistColorGreen: array[0..ciMaxBits-1] of Integer;
   iaHistColorBlue: array[0..ciMaxBits-1] of Integer;
   iCnt: Integer;
   iMax: Integer;
begin
   if(not F__OPENFITS.mbNewLoaded) then
   begin
     F__HISTOGRAM.ShowModal;
     exit;
   end;

   SrcIntfImg:=TLazIntfImage.Create(0,0);
   SrcIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);

   for i:=0 to ciMaxBits-1 do
   begin
     iaHistColorRed[i] := 0;
     iaHistColorGreen[i] := 0;
     iaHistColorBlue[i] := 0;
   end;

   iCnt := 0; iMax := 0;
   for py:=0 to SrcIntfImg.Height-1 do
   begin
     for px:=0 to SrcIntfImg.Width-1 do
     begin

       CurColor:=SrcIntfImg.Colors[px,py];
       // Omit black pixel
       if(CurColor.Red > 0) or (CurColor.Green > 0) or (CurColor.Blue > 0) then
       begin
         Inc(iCnt);

         iaHistColorRed[CurColor.Red] := iaHistColorRed[CurColor.Red] + 1;
         iaHistColorGreen[CurColor.Green] := iaHistColorGreen[CurColor.Green] + 1;
         iaHistColorBlue[CurColor.Blue] := iaHistColorBlue[CurColor.Blue] + 1;

         if(iaHistColorRed[CurColor.Red] > iMax) then iMax := iaHistColorRed[CurColor.Red];
         if(iaHistColorGreen[CurColor.Green] > iMax) then iMax := iaHistColorGreen[CurColor.Green];
         if(iaHistColorBlue[CurColor.Blue] > iMax) then iMax := iaHistColorBlue[CurColor.Blue];

       end;

     end;
   end;

   SrcIntfImg.Free;

   if(iCnt > 0) and (iMax > 0) then
   begin
     (F__HISTOGRAM.CHART.Series[0] as TLineSeries).Clear;
     (F__HISTOGRAM.CHART.Series[1] as TLineSeries).Clear;
     (F__HISTOGRAM.CHART.Series[2] as TLineSeries).Clear;
     (F__HISTOGRAM.CHART.Series[3] as TLineSeries).Clear;
     //(F__HISTOGRAM.CHART.Series[4] as TConstantSeries).Clear;
     //(F__HISTOGRAM.CHART.Series[5] as TConstantSeries).Clear;

     for i:=0 to ciMaxBits-1 do
     begin
       (F__HISTOGRAM.CHART.Series[0] as TLineSeries).AddXY(i,iaHistColorRed[i]/iMax * 100.0);
       (F__HISTOGRAM.CHART.Series[1] as TLineSeries).AddXY(i,iaHistColorGreen[i]/iMax * 100.0);
       (F__HISTOGRAM.CHART.Series[2] as TLineSeries).AddXY(i,iaHistColorBlue[i]/iMax * 100.0);
        case F__HISTOGRAM.PC__CONTROL.ActivePageIndex of
         0: (F__HISTOGRAM.CHART.Series[3] as TLineSeries).AddXY(i,1.0*i/(ciMaxBits-1) * 100.0);
         1: (F__HISTOGRAM.CHART.Series[3] as TLineSeries).AddXY(i,50.0);
        end;
     end;

     IniText(F__HISTOGRAM,msLANG_ID);

     F__HISTOGRAM.ShowModal;
   end;
end;

procedure TF__OPENFITS.OpenFitsMenuClick(Sender: TObject);
{2020/02/23 / fs
Opens the correponding image which is addressed via the menu.tag
}
var
  iIndex: Integer;
begin
  iIndex := (Sender as TMenuItem).Tag;
  if(iIndex > -1) and (iIndex < mslImgLst.Count) then
  begin
    (mslImgLst.Objects[iIndex] as TF__IMG).WindowState:=wsNormal;
    (mslImgLst.Objects[iIndex] as TF__IMG).Show;
  end;
end;

procedure TF__OPENFITS.SaveImg();
{2020/02/23 / fs
Saves an image which is addressed by the miActiveForm index.
}
begin
  if(SDLG__IMG.Execute) and (Trim(SDLG__IMG.FileName) <> '') and (miActiveImgForm > -1) and (miActiveImgForm < mslImgLst.Count) then
  begin
    (mslImgLst.Objects[miActiveImgForm] as TF__IMG).IMG.Picture.SaveToFile(SDLG__IMG.FileName);
    mbNewLoaded := true;
  end;
end;

procedure TF__OPENFITS.GetImgWindowPos(iIndex: Integer; var iLeft: Integer; var iTop: Integer);
{2020/03/17 / fs
Calculates to top-left corner of client window identified with index 'iIndex'.
}
begin
  iLeft:=10*(iIndex+1);
  iTop:=ciWindowPosOffset + Height + 10*(iIndex+1);
end;

procedure TF__OPENFITS.MENU__FILEOPENClick(Sender: TObject);
{2020/02/23 / fs
Opens the selected image via the fileopen dialogue.
After opening a corresponding menue item is generated to re-open the image.
}
var
  iIndex: Integer;
  F__IMG: TF__IMG;
  MenuItem: TMenuItem;
begin
  iIndex := -1;

  if(ODLG__OPENFITS.Execute) and (Trim(ODLG__OPENFITS.FileName) <> '') then
  begin
    F__IMG := NewImgWindow(ODLG__OPENFITS.FileName,iIndex);

    miActiveImgForm := iIndex;

    if(RightStr(Lowercase(ODLG__OPENFITS.FileName),4) = '.fit') then
    begin
      // Init object
      FFit := TFitsFileBitmap.CreateJoin(ODLG__OPENFITS.FileName, cFileRead);
      // Init region
      FRgn.X1 := 0;
      FRgn.Y1 := 0;
      FRgn.Width  := F__IMG.IMG.Width;
      FRgn.Height := F__IMG.IMG.Height;

      FFit.BitmapRead(F__IMG.IMG.Picture.Bitmap, FRgn);
    end
    else
    begin
      F__IMG.IMG.Picture.LoadFromFile(ODLG__OPENFITS.FileName);
    end;

    RegisterFloatBitmap(F__IMG.IMG.Picture.Bitmap,miActiveImgForm);

    F__IMG.Show;

    // Generate corresponding menu item for re-opening
    MenuItem := TMenuItem.Create(MMENU__OPENFITS);
    MenuItem.Caption:=ODLG__OPENFITS.FileName;
    MenuItem.OnClick:=@OpenFitsMenuClick;
    MenuItem.Tag:=iIndex;
    MENU__WINDOW.Add(MenuItem);

  end;
end;

procedure TF__OPENFITS.MENU__HISTOGRAMClick(Sender: TObject);
{2020/02/23 / fs
Activates the Histogram using an image which is addressed by miActiveImgForm index
}
begin
if(miActiveImgForm > -1) and (miActiveImgForm < mslImgLst.Count) and
  ((mslImgLst.Objects[miActiveImgForm] as TF__IMG).IMG.Picture.Width > 0) then
  begin
    GetHistogramData((mslImgLst.Objects[miActiveImgForm] as TF__IMG).IMG.Picture.Bitmap);
    L__TEST.Caption:='---';
  end;
end;

procedure TF__OPENFITS.MENU__INFOClick(Sender: TObject);
{2020/02/23 / fs
Displays the info-dialogue for OpenFits
}
begin
  F__INFO := TF__INFO.Create(nil);
  IniText(F__INFO,msLANG_ID);
  F__INFO.ShowModal;
  F__INFO.Free;
end;

procedure TF__OPENFITS.MENU__LANG_DEClick(Sender: TObject);
{2020/02/23 / fs
Menu code for switching into German language
}
begin
  msLANG_ID := 'DE';
  IniText(F__OPENFITS,msLANG_ID);
  MENU__LANG_DE.Checked := true;
  MENU__LANG_EN.Checked := false;
end;

procedure TF__OPENFITS.MENU__LANG_ENClick(Sender: TObject);
{2020/02/23 / fs
Menu code for switching into English language
}
begin
  msLANG_ID := 'EN';
  IniText(F__OPENFITS,msLANG_ID);
  MENU__LANG_DE.Checked := false;
  MENU__LANG_EN.Checked := true;
end;

procedure TF__OPENFITS.MENU__MINIMIZE_ALLClick(Sender: TObject);
{2020/02/23 / fs
Menu code ti minimize all opended image windows
}
var
  i: Integer;
begin
  for i:=0 to mslImgLst.Count-1 do
  begin
    if(mslImgLst.Objects[i] <> nil) then
    begin
      (mslImgLst.Objects[i] as TF__IMG).WindowState:=wsMinimized;
    end;
  end;

end;

procedure TF__OPENFITS.MENU__SAVEClick(Sender: TObject);
{2020/02/23 / fs
Menu code for saving the current image
}
begin
  SaveImg();
end;

procedure TF__OPENFITS.MENU__WINDOW_OVERLAPClick(Sender: TObject);
{2020/02/23 / fs
Menu code to show all opened images in overlapped order
}
var
  i: Integer;
begin
  for i:=0 to mslImgLst.Count-1 do
  begin
    if(mslImgLst.Objects[i] <> nil) then
    begin
      (mslImgLst.Objects[i] as TF__IMG).Left := 20*(i+1);
      (mslImgLst.Objects[i] as TF__IMG).Top := ciWindowPosOffset + Height + 20*(i+1);
    end;
  end;
end;

procedure TF__OPENFITS.MENU__WINDOW_SIDEBYSIDEClick(Sender: TObject);
{2020/02/23 / fs
Menu code to show all opened images in side-by-side order
}
var
  i: Integer;
  iWindowWidth, iLeft: Integer;
begin
  if(mslImgLst.Count = 0) then
    exit;

  iWindowWidth := Screen.Width div mslImgLst.Count;
  iWindowWidth := iWindowWidth - 2;
  iLeft := ciWindowPosOffset;

  for i:=0 to mslImgLst.Count-1 do
  begin
    if(mslImgLst.Objects[i] <> nil) then
    begin
      (mslImgLst.Objects[i] as TF__IMG).Top := ciWindowPosOffset + Height;
      (mslImgLst.Objects[i] as TF__IMG).Width := iWindowWidth;
      (mslImgLst.Objects[i] as TF__IMG).Left := iLeft;
      iLeft := iLeft + iWindowWidth;
    end;
  end;
end;

procedure TF__OPENFITS.MENU__WINDOW_STACKEDClick(Sender: TObject);
{2020/02/23 / fs
Menu code to show all opened images in stacked order
}
var
  i: Integer;
  iWindowHeight, iTop: Integer;
begin
  if(mslImgLst.Count = 0) then
    exit;

  iWindowHeight := (Screen.Height - ciWindowPosOffset) div mslImgLst.Count;
  iWindowHeight := iWindowHeight - 2;
  iTop := ciWindowPosOffset;

  for i:=0 to mslImgLst.Count-1 do
  begin
    if(mslImgLst.Objects[i] <> nil) then
    begin
      (mslImgLst.Objects[i] as TF__IMG).Left := ciWindowPosOffset;;

      (mslImgLst.Objects[i] as TF__IMG).Top := iTop + Height;
      (mslImgLst.Objects[i] as TF__IMG).Height := iWindowHeight;
      iTop := iTop + iWindowHeight;
    end;
  end;
end;

procedure TF__OPENFITS.FormCreate(Sender: TObject);
{2020/02/23 / fs
All things to do when OpenFits is created:
- Initialize varaibles
- Creating objects
- Initialize language dependent texts
}
begin
  mslImgLst := TStringList.Create;
  miActiveImgForm := -1;
  mSrcIntfImg:=TLazIntfImage.Create(0,0);
  msLANG_ID := 'DE';


  IniText(F__OPENFITS,'DE');
end;

procedure TF__OPENFITS.FormDestroy(Sender: TObject);
{2020/02/23 / fs
All things to do when OpenFits is detroyed/freed e.g. freeing objects
}
var
  i: Integer;
begin
  mSrcIntfImg.Free;

  for i:=0 to mslImgLst.Count-1 do
  begin
    (mslImgLst.Objects[i] as TF__IMG).Free;
    mslImgLst.Objects[i] := nil;
  end;

  mslImgLst.Destroy;
end;

procedure TF__OPENFITS.FormShow(Sender: TObject);
{2020/02/23 / fs
Things to do if the form is focused: Setting initial values of the window
}
begin
  Top := 0; Left := 0;
  Width := Screen.Width;
end;

procedure TF__OPENFITS.MENU__ADAPT_SIZEClick(Sender: TObject);
{2020/02/23 / fs
Menu code to hamonize the window dimension with the displayed image of the selected windows identified
with index miActiveImgForm
}
begin
  if(miActiveImgForm > -1) and (miActiveImgForm < mslImgLst.Count) and
    ((mslImgLst.Objects[miActiveImgForm] as TF__IMG).IMG.Picture.Width > 0) then
  begin
    (mslImgLst.Objects[miActiveImgForm] as TF__IMG).Height :=
      (mslImgLst.Objects[miActiveImgForm] as TF__IMG).Width *
        (mslImgLst.Objects[miActiveImgForm] as TF__IMG).IMG.Picture.Height div (mslImgLst.Objects[miActiveImgForm] as TF__IMG).IMG.Picture.Width;
  end;
end;

procedure TF__OPENFITS.MENU__ADAPT_SIZE_ALLClick(Sender: TObject);
{2020/02/23 / fs
Menu code to hamonize window dimension with the displayed image carried out on all opened windows
}
var
  iIndex: Integer;
begin
  for iIndex:=0 to mslImgLst.Count-1 do
  begin
    if((mslImgLst.Objects[iIndex] as TF__IMG).IMG.Picture.Width > 0) then
    begin
      (mslImgLst.Objects[iIndex] as TF__IMG).Height :=
        (mslImgLst.Objects[iIndex] as TF__IMG).Width *
          (mslImgLst.Objects[iIndex] as TF__IMG).IMG.Picture.Height div (mslImgLst.Objects[iIndex] as TF__IMG).IMG.Picture.Width;
    end;
  end; // for iIndex..

end;

procedure TF__OPENFITS.MENU__CLONEClick(Sender: TObject);
var
  F__IMG: TF__IMG;
  iNewIndex: Integer;
begin
  iNewIndex := -1;
  F__IMG := CloneImgWindow(miActiveImgForm, iNewIndex);
  if(F__IMG <> nil) and (iNewIndex > -1) then
  begin
    F__IMG.Show;
    miActiveImgForm := iNewIndex;
  end;
end;


procedure TF__OPENFITS.MENU__CLOSEClick(Sender: TObject);
{2020/02/23 / fs
Menu code to close the OpenFits main window
}
begin
  Close;
end;

procedure TF__OPENFITS.MENU__EDIT_ADDClick(Sender: TObject);
begin
  (mslImgLst.Objects[miActiveImgForm] as TF__IMG).mbModBuffer := true;
  ExecBasicImgCalc(ofpAdd);
  (mslImgLst.Objects[miActiveImgForm] as TF__IMG).mbModBuffer := false;
end;

procedure TF__OPENFITS.MENU__EDIT_DIVIDEClick(Sender: TObject);
begin
  (mslImgLst.Objects[miActiveImgForm] as TF__IMG).mbModBuffer := true;
  ExecBasicImgCalc(ofpDiv);
  (mslImgLst.Objects[miActiveImgForm] as TF__IMG).mbModBuffer := false;
end;

procedure TF__OPENFITS.MENU__EDIT_LIMITLOWClick(Sender: TObject);
begin
  (mslImgLst.Objects[miActiveImgForm] as TF__IMG).mbModBuffer := true;
  ExecBasicImgCalc(ofpCutLow);
  (mslImgLst.Objects[miActiveImgForm] as TF__IMG).mbModBuffer := false;
end;

procedure TF__OPENFITS.MENU__EDIT_MINUSClick(Sender: TObject);
begin
  (mslImgLst.Objects[miActiveImgForm] as TF__IMG).mbModBuffer := true;
  ExecBasicImgCalc(ofpMinus);
  (mslImgLst.Objects[miActiveImgForm] as TF__IMG).mbModBuffer := false;
end;

procedure TF__OPENFITS.MENU__EDIT_MULTClick(Sender: TObject);
begin
  (mslImgLst.Objects[miActiveImgForm] as TF__IMG).mbModBuffer := true;
  ExecBasicImgCalc(ofpMult);
  (mslImgLst.Objects[miActiveImgForm] as TF__IMG).mbModBuffer := false;
end;


end.

