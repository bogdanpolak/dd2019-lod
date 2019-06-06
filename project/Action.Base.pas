{ * ------------------------------------------------------------------------
  * ♥  Bogdan Polak © 2019  ♥
  *  ----------------------------------------------------------------------- * }
unit Action.Base;

interface

uses
  System.Classes,
  Vcl.ActnList,
  System.Actions;

type
  TBaseAction = class (TAction)
  private
    procedure EventOnExecute (Sender: TObject);
  protected
    procedure DoInitialiaze; virtual; abstract;
    procedure DoExecute; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation


constructor TBaseAction.Create(AOwner: TComponent);
begin
  inherited;
  DoInitialiaze;
  Self.OnExecute := EventOnExecute;
end;

procedure TBaseAction.EventOnExecute(Sender: TObject);
begin
  DoExecute;
end;

end.
