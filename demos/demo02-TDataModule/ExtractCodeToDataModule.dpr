program ExtractCodeToDataModule;

uses
  Vcl.Forms,
  Module.Main in 'Module.Main.pas' {MainModule: TDataModule},
  Form.Main in 'Form.Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainModule, MainModule);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
