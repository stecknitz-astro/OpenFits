program OpenFits;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, DeLaFitsClasses, U_OpenFits, U_ImageForm,
  u_histogram, u_const, u_info, U_Translation
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TF__OPENFITS, F__OPENFITS);
  Application.CreateForm(TF__HISTOGRAM, F__HISTOGRAM);
  Application.Run;
end.

