{ * ------------------------------------------------------------------------
  * ♥  Bogdan Polak © 2019  ♥
  *  ----------------------------------------------------------------------- * }
unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.Messaging,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ComCtrls,
  Plus.Vcl.PageControlFactory,
  Plus.Vcl.ActionGuiBuilder,
  Frame.Console;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    PageControl1: TPageControl;
    procedure FormCreate(Sender: TObject);
  private
    PageControlFactory: TPageControlFactory;
    FFrameConsole: TFrameConsole;
    procedure OnConsoleWriteMessage(const Sender: TObject;
      const M: System.Messaging.TMessage);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Action.Console.HelloWorld;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;
  PageControlFactory := TPageControlFactory.Create(Self);
  PageControlFactory.PageControl := PageControl1;
  TActionGuiBuilder.Create(Self).BuildButtonsFromActions(GroupBox1, [
    THelloWorldAction.Create(Self)
  ]);
  TMessageManager.DefaultManager.SubscribeToMessage(TMessage<UnicodeString>,
    OnConsoleWriteMessage);
end;

procedure TForm1.OnConsoleWriteMessage(const Sender: TObject;
  const M: System.Messaging.TMessage);
var
  ACaption: string;
begin
  if FFrameConsole = nil then
  begin
    ACaption := 'Console write log';
    FFrameConsole := PageControlFactory.CreateFrame<TFrameConsole>(ACaption);
    FFrameConsole.OnCloseFrame := (
      procedure(Sender: TFrame)
      begin
        FFrameConsole := nil;
        (Sender.Owner as TTabSheet).Free;
      end);
  end;
  FFrameConsole.DoConsoleWrite((M as TMessage<UnicodeString>).Value);
end;

end.
