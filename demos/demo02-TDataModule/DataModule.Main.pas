unit DataModule.Main;

interface

uses
  System.SysUtils, System.Classes,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.ConsoleUI.Wait, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Comp.UI,
  Model.Employee;

type
  TMainDataModule = class(TDataModule)
    FDConnection1: TFDConnection;
    fdqEmployees: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
  private
  public
    procedure ForEachEmployee (proc: TProc<TEmployee> );
  end;

var
  MainDataModule: TMainDataModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ TMainDataModule }

procedure TMainDataModule.ForEachEmployee(proc: TProc<TEmployee>);
var
  mode: TFDFetchMode;
  emp: TEmployee;
begin
  if Assigned(proc) then begin
    fdqEmployees.Close;
    fdqEmployees.FetchOptions.Mode := fmAll;
    fdqEmployees.Open();
    try
      emp := TEmployee.Create;
      try
        while not fdqEmployees.Eof do
        begin
          emp.ID := fdqEmployees.FieldByName('EmployeeID').AsInteger;
          emp.TitleOfCourtesy := fdqEmployees.FieldByName('TitleOfCourtesy').AsString;
          emp.FirstName := fdqEmployees.FieldByName('FirstName').AsString;
          emp.LastName := fdqEmployees.FieldByName('LastName').AsString;
          proc (emp);
          fdqEmployees.Next;
        end;
      finally
        emp.Free;
      end;
    finally
      fdqEmployees.Close();
    end;
  end;
end;

end.
