{ * ------------------------------------------------------------------------
  * ♥  Bogdan Polak © 2019  ♥
  *  ----------------------------------------------------------------------- * }
unit Action.Console;

interface

uses
  System.Classes,
  Vcl.ActnList,
  System.Actions;

type
  TConsoleAction = class (TAction)
  private
    procedure EventOnExecute (Sender: TObject);
  protected
    procedure DoInitialiaze; virtual; abstract;
    procedure DoExecute; virtual; abstract;
    procedure ConsoleWrite(const s: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  System.Messaging;


constructor TConsoleAction.Create(AOwner: TComponent);
begin
  inherited;
  DoInitialiaze;
  Self.OnExecute := EventOnExecute;
end;

procedure TConsoleAction.EventOnExecute(Sender: TObject);
begin
  DoExecute;
end;

procedure TConsoleAction.ConsoleWrite(const s: string);
begin
  TMessageManager.DefaultManager.SendMessage(Self,
    TMessage<UnicodeString>.Create(s), True);
end;

end.
