program DemoTConsoleAction;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {Form1},
  Action.Console in 'Action.Console.pas',
  Plus.Vcl.ActionGuiBuilder in 'Plus.Vcl.ActionGuiBuilder.pas',
  Plus.Vcl.PageControlFactory in 'Plus.Vcl.PageControlFactory.pas',
  Frame.Console in 'Frame.Console.pas' {FrameConsole: TFrame},
  Action.Console.HelloWorld in 'Action.Console.HelloWorld.pas',
  Action.SpringDemo.Lists in 'Action.SpringDemo.Lists.pas',
  Action.SpringDemo.Logger in 'Action.SpringDemo.Logger.pas',
  Plus.Vcl.Timer in 'Plus.Vcl.Timer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
