unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses DataModule.Main;

procedure TForm1.Button1Click(Sender: TObject);
var
  title: string;
  firstName: string;
  lastName: string;
begin
  with MainDataModule do begin
    fdqEmployees.Open();
    try
      fdqEmployees.FetchAll;
      while not fdqEmployees.Eof do
      begin
        title := fdqEmployees.FieldByName('TitleOfCourtesy').AsString;
        firstName := fdqEmployees.FieldByName('FirstName').AsString;
        lastName := fdqEmployees.FieldByName('LastName').AsString;
        Memo1.Lines.Add( Format('[%d] %s',[
          fdqEmployees.FieldByName('EmployeeID').AsInteger,
          title +' '+ firstName + ' ' + lastName]) );
        fdqEmployees.Next;
      end;
    finally
      fdqEmployees.Close();
    end;
  end;
end;

end.
