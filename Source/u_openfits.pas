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
  ciWindowPosOffset = 40;

type

  { TF__OPENFITS }

  TF__OPENFITS = class(TForm)
    IMGL__LIB: TImageList;
    IMGL__MENU: TImageList;
    L__TEST: TLabel;
    MenuItem1: TMenuItem;
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
    procedure MENU__CLOSEClick(Sender: TObject);
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
    FFit: TFitsFileBitmap;
    FRgn: TRgn;
    mslImgLst: TStringList;
    mSrcIntfImg: TLazIntfImage; // Image modification memory

    marFloatBitmap_R: array of array of Real;
    marFloatBitmap_G: array of array of Real;
    marFloatBitmap_B: array of array of Real;
    miXDim, miYDim: Integer;

    procedure SaveImg();
    procedure OpenFitsMenuClick(Sender: TObject);
    procedure GetHistogramData(ABitMap: Graphics.TBitmap);
    function GetGammaVal(rIntensity: Real; rGamma: Real): Word;
    function GetHistContrastVal(rIntensity: Real; rHist: Real): Word;
    procedure RegisterFloatBitmap(ABitMap: Graphics.TBitmap);

  public
    miActiveImgForm: Integer;
    msLANG_ID: string;
    mbNewLoaded: Boolean;

    procedure ModifyActiveImage(rArg: Real; OpenFitsPixFunction: TOpenFitsPixFunction; bRed, bGreen, bBlue: Boolean);
    procedure UnregisterFloatBitmap();

  end;

var
  F__OPENFITS: TF__OPENFITS;

type
  TRGBTripleArray = array[0..32767] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;

implementation

{$R *.lfm}

{ TF__OPENFITS }

procedure TF__OPENFITS.UnregisterFloatBitmap();
begin
  SetLength(marFloatBitmap_R, 0, 0);
  SetLength(marFloatBitmap_G, 0, 0);
  SetLength(marFloatBitmap_B, 0, 0);

  miXDim := 0;
  miYDim := 0;
end;

function TF__OPENFITS.GetHistContrastVal(rIntensity: Real; rHist: Real): Word;
var
  rX, rY: Real;
const
  rcXMax = 10.0;
begin
  rX := rIntensity*rcXMax/ciBit_16; // [0..10]!
  rY := (0.5*Pi + arctan2((rX - rcXMax/2)*rHist,1))/Pi *ciBit_16;

  Result := Trunc(rY);
end;


function TF__OPENFITS.GetGammaVal(rIntensity: Real; rGamma: Real): Word;
var
  rX, rY: Real;
begin
  rX := rIntensity/ciBit_16; // [0..1]!
  rY := Power(rX,1.0/rGamma) *ciBit_16;

  Result := Trunc(rY);
end;


procedure TF__OPENFITS.ModifyActiveImage(rArg: Real; OpenFitsPixFunction: TOpenFitsPixFunction; bRed, bGreen, bBlue: Boolean);
var
  TempIntfImg: TLazIntfImage;
  ImgHandle,ImgMaskHandle: HBitmap;
  pngbmp: TPortableNetworkGraphic;
  px, py: Integer;
  CurColor: TFPColor;
  MemoryStream: TMemoryStream;
  ABitMap: Graphics.TBitmap;
begin

  if(miActiveImgForm < 0) or (miActiveImgForm >= mslImgLst.Count) then
    exit;

  L__TEST.Caption:=FloatToStrF(rArg,ffFixed,8,2);

  // Load active picture indexed by 'miActiveImgForm'
  ABitMap := (mslImgLst.Objects[miActiveImgForm] as TF__IMG).IMG.Picture.Bitmap;

  if(ABitMap = nil) then
    exit;


  if(miXDim = 0) or (miYDim = 0) then
    exit;

  pngbmp := TPortableNetworkGraphic.Create;
  MemoryStream := TMemoryStream.Create;

  TempIntfImg:=TLazIntfImage.Create(0,0);
  TempIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);

  try
    Screen.Cursor:=crHourGlass;

    for py:=0 to miYDim-1 do begin
      for px:=0 to miXDim-1 do begin

        CurColor.red:=Trunc(marFloatBitmap_R[px,py]);
        CurColor.green:=Trunc(marFloatBitmap_G[px,py]);
        CurColor.blue:=Trunc(marFloatBitmap_B[px,py]);

        case OpenFitsPixFunction of
          ofpGamma:
          begin
            if(bRed) then CurColor.Red:=GetGammaVal(marFloatBitmap_R[px,py],rArg);
            if(bGreen) then CurColor.Green:=GetGammaVal(marFloatBitmap_G[px,py],rArg);
            if(bBlue) then CurColor.Blue:=GetGammaVal(marFloatBitmap_B[px,py],rArg);
          end;
          ofpHist:
          begin
            if(bRed) then CurColor.Red:=GetHistContrastVal(marFloatBitmap_R[px,py],rArg);
            if(bGreen) then CurColor.Green:=GetHistContrastVal(marFloatBitmap_G[px,py],rArg);
            if(bBlue) then CurColor.Blue:=GetHistContrastVal(marFloatBitmap_B[px,py],rArg);
          end;
        end; // case

        TempIntfImg.Colors[px,py]:=CurColor;
      end;
    end;
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

procedure TF__OPENFITS.RegisterFloatBitmap(ABitMap: Graphics.TBitmap);
var
   SrcIntfImg: TLazIntfImage;
   px, py: Integer;
   CurColor: TFPColor;
begin
  mbNewLoaded := true; // Used to initialize working dialogs like Histogram, ...

  SrcIntfImg:=TLazIntfImage.Create(0,0);
  SrcIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);

  SetLength(marFloatBitmap_R, SrcIntfImg.Width, SrcIntfImg.Height);
  SetLength(marFloatBitmap_G, SrcIntfImg.Width, SrcIntfImg.Height);
  SetLength(marFloatBitmap_B, SrcIntfImg.Width, SrcIntfImg.Height);

  miXDim := SrcIntfImg.Width;
  miYDim := SrcIntfImg.Height;

  for py:=0 to SrcIntfImg.Height-1 do
  begin
    for px:=0 to SrcIntfImg.Width-1 do
    begin
      CurColor:=SrcIntfImg.Colors[px,py];

      marFloatBitmap_R[px,py] := CurColor.red;
      marFloatBitmap_G[px,py] := CurColor.green;
      marFloatBitmap_B[px,py] := CurColor.blue;

    end;
  end;

  SrcIntfImg.Free;

end;

procedure TF__OPENFITS.GetHistogramData(ABitMap: Graphics.TBitmap);
var
   SrcIntfImg: TLazIntfImage;
   i,px, py: Integer;
   CurColor: TFPColor;
   iaHistColorRed: array[0..ciBit_16] of Integer;
   iaHistColorGreen: array[0..ciBit_16] of Integer;
   iaHistColorBlue: array[0..ciBit_16] of Integer;
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

   for i:=0 to ciBit_16 do
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

     for i:=0 to ciBit_16-1 do
     begin
       (F__HISTOGRAM.CHART.Series[0] as TLineSeries).AddXY(i,iaHistColorRed[i]/iMax * 100.0);
       (F__HISTOGRAM.CHART.Series[1] as TLineSeries).AddXY(i,iaHistColorGreen[i]/iMax * 100.0);
       (F__HISTOGRAM.CHART.Series[2] as TLineSeries).AddXY(i,iaHistColorBlue[i]/iMax * 100.0);
        case F__HISTOGRAM.PC__CONTROL.ActivePageIndex of
         0: (F__HISTOGRAM.CHART.Series[3] as TLineSeries).AddXY(i,1.0*i/ciBit_16 * 100.0);
         1: (F__HISTOGRAM.CHART.Series[3] as TLineSeries).AddXY(i,50.0);
        end;
     end;

     IniText(F__HISTOGRAM,msLANG_ID);

     F__HISTOGRAM.ShowModal;
   end;
end;

procedure TF__OPENFITS.OpenFitsMenuClick(Sender: TObject);
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
begin
  if(SDLG__IMG.Execute) and (Trim(SDLG__IMG.FileName) <> '') and (miActiveImgForm > -1) and (miActiveImgForm < mslImgLst.Count) then
  begin
    (mslImgLst.Objects[miActiveImgForm] as TF__IMG).IMG.Picture.SaveToFile(SDLG__IMG.FileName);
    mbNewLoaded := true;
  end;
end;

procedure TF__OPENFITS.MENU__FILEOPENClick(Sender: TObject);
var
  sLowFileName: string;
  iIndex: Integer;
  F__IMG: TF__IMG;
  MenuItem: TMenuItem;
begin
  if(ODLG__OPENFITS.Execute) and (Trim(ODLG__OPENFITS.FileName) <> '') then
  begin
    sLowFileName := Lowercase(ODLG__OPENFITS.FileName);

    F__IMG := TF__IMG.Create(F__OPENFITS);
    mslImgLst.AddObject('F__IMG_' + IntToStr(mslImgLst.Count+1), F__IMG);
    iIndex := mslImgLst.Count-1;
    F__IMG.Name:='F__IMG_' + IntToStr(mslImgLst.Count+1);
    F__IMG.Tag := iIndex;
    F__IMG.Caption:=ODLG__OPENFITS.FileName;

    if(RightStr(sLowFileName,4) = '.fit') then
    begin
      // Init object
      FFit := TFitsFileBitmap.CreateJoin(ODLG__OPENFITS.FileName, cFileRead);
      // Init region
      FRgn.X1 := 0;
      FRgn.Y1 := 0;
      FRgn.Width  := F__IMG.IMG.Width;
      FRgn.Height := F__IMG.IMG.Height;

      FFit.BitmapRead(F__IMG.IMG.Picture.Bitmap, FRgn);
      //FFit.BitmapRead(F__IMG.IMG__HIDDEN.Picture.Bitmap, FRgn);
    end
    else
    begin
      F__IMG.IMG.Picture.LoadFromFile(ODLG__OPENFITS.FileName);
      //F__IMG.IMG__HIDDEN.Picture.LoadFromFile(ODLG__OPENFITS.FileName);
    end;

    RegisterFloatBitmap(F__IMG.IMG.Picture.Bitmap);

    F__IMG.Left:=10*(iIndex+1);
    F__IMG.Top:=ciWindowPosOffset + Height + 10*(iIndex+1);

    F__IMG.Show;

    MenuItem := TMenuItem.Create(MMENU__OPENFITS);
    MenuItem.Caption:=ODLG__OPENFITS.FileName;
    MenuItem.OnClick:=@OpenFitsMenuClick;
    MenuItem.Tag:=iIndex;
    MENU__WINDOW.Add(MenuItem);

    // Load image map into modification memory
    //SrcIntfImg:=TLazIntfImage.Create(0,0);
    //SrcIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);

  end;
end;

procedure TF__OPENFITS.MENU__HISTOGRAMClick(Sender: TObject);
begin
if(miActiveImgForm > -1) and (miActiveImgForm < mslImgLst.Count) and
  ((mslImgLst.Objects[miActiveImgForm] as TF__IMG).IMG.Picture.Width > 0) then
  begin
    GetHistogramData((mslImgLst.Objects[miActiveImgForm] as TF__IMG).IMG.Picture.Bitmap);
    L__TEST.Caption:='---';
  end;
end;

procedure TF__OPENFITS.MENU__INFOClick(Sender: TObject);
begin
  F__INFO := TF__INFO.Create(nil);
  IniText(F__INFO,msLANG_ID);
  F__INFO.ShowModal;
  F__INFO.Free;
end;

procedure TF__OPENFITS.MENU__LANG_DEClick(Sender: TObject);
begin
  msLANG_ID := 'DE';
  IniText(F__OPENFITS,msLANG_ID);
  MENU__LANG_DE.Checked := true;
  MENU__LANG_EN.Checked := false;
end;

procedure TF__OPENFITS.MENU__LANG_ENClick(Sender: TObject);
begin
  msLANG_ID := 'EN';
  IniText(F__OPENFITS,msLANG_ID);
  MENU__LANG_DE.Checked := false;
  MENU__LANG_EN.Checked := true;
end;

procedure TF__OPENFITS.MENU__MINIMIZE_ALLClick(Sender: TObject);
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
begin
  SaveImg();
end;

procedure TF__OPENFITS.MENU__WINDOW_OVERLAPClick(Sender: TObject);
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
begin
  mslImgLst := TStringList.Create;
  miActiveImgForm := -1;
  mSrcIntfImg:=TLazIntfImage.Create(0,0);

  IniText(F__OPENFITS,'DE');
end;

procedure TF__OPENFITS.FormDestroy(Sender: TObject);
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
begin
  Top := 0; Left := 0;
  Width := Screen.Width;
end;

procedure TF__OPENFITS.MENU__ADAPT_SIZEClick(Sender: TObject);
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


procedure TF__OPENFITS.MENU__CLOSEClick(Sender: TObject);
begin
  Close;
end;

end.

