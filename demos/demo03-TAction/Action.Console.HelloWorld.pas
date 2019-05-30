unit Action.Console.HelloWorld;

interface

uses
  Action.Console;

type
  THelloWorldAction = class(TConsoleAction)
    procedure DoInitialiaze; override;
    procedure DoExecute; override;
  end;

implementation

uses
  Vcl.Menus;

procedure THelloWorldAction.DoInitialiaze;
begin
  Caption := 'Hello world (Ctrl+1)';
  ShortCut := Vcl.Menus.TextToShortCut('Ctrl+1');
end;

procedure THelloWorldAction.DoExecute;
begin
  ConsoleWrite('Hello world !!!');
end;

end.
