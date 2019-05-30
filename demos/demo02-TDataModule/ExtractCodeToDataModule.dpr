program ExtractCodeToDataModule;

uses
  Vcl.Forms,
  DataModule.Main in 'DataModule.Main.pas' {MainDataModule: TDataModule},
  Form.Main in 'Form.Main.pas' {Form1},
  Model.Employee in 'Model.Employee.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainDataModule, MainDataModule);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
