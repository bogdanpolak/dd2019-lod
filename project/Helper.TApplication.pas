unit Helper.TApplication;

interface

uses
  System.SysUtils, Vcl.Forms;

type
  TApplicationHelper = class helper for TApplication
    function IsDeveloperMode :boolean;
  end;

implementation

{ TApplicationHelper }

function TApplicationHelper.IsDeveloperMode: boolean;
var
  Extention: string;
  ExeFileName: string;
  ProjectFileName: string;
begin
{$IFDEF DEBUG}
  Extention := '.dpr';
  ExeFileName := ExtractFileName(Application.ExeName);
  ProjectFileName := ChangeFileExt(ExeFileName, Extention);
  Result := FileExists(ProjectFileName) or
    FileExists('..\..\' + ProjectFileName);
{$ELSE}
  Result := False;
{$ENDIF}
end;

end.
