unit Action.SpringDemo;

interface

uses
  Action.Console;

type
  TSpringAction = class (TConsoleAction)
    procedure DoInitialiaze; override;
    procedure DoExecute; override;
  end;

implementation

{ TSpringAction }

procedure TSpringAction.DoInitialiaze;
begin
  Caption := Write action caption;
end;

procedure TSpringAction.DoExecute;
begin
  // TODO:
  ConsoleWrite('--------------------------');
end;

end.
