unit Model.Employee;

interface

type
  TEmployee = class
  public
    ID: integer;
    TitleOfCourtesy: string;
    FirstName: string;
    LastName: string;
    function FullName: string;
  end;

implementation

function TEmployee.FullName: string;
begin
  Result := TitleOfCourtesy +' '+ FirstName + ' ' + LastName;
end;

end.
