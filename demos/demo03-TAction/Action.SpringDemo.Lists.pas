{ * ------------------------------------------------------------------------
  * ♥  Bogdan Polak © 2019  ♥
  *  ----------------------------------------------------------------------- * }
unit Action.SpringDemo.Lists;

interface

uses
  System.SysUtils,
  Action.Console;

type
  TSpringListAction = class(TConsoleAction)
    procedure DoInitialiaze; override;
    procedure DoExecute; override;
  end;

implementation

// ------------------------------------------------------------------------
// ------------------------------------------------------------------------
// Demo: Spring4D List manipulation, using TSelectManyIterator
// * IList<Integer>
// * OuterList: IList<IEnumerable<Integer>>
// * TEnumerableHelper.SelectMany<Integer>
// ------------------------------------------------------------------------

uses
  Spring,
  Spring.Collections,
  Spring.Data.ObjectDataSet,
  Spring.Collections.Extensions;

procedure TSpringListAction.DoInitialiaze;
begin
  Caption := 'Spring4D List manipulation';
end;

function JoinEnumerable(const separator: string;
  const values: Spring.Collections.IEnumerable<Integer>): string;
var
  e: Spring.Collections.IEnumerator<Integer>;
begin
  e := values.GetEnumerator;
  if not e.MoveNext then
    Exit('');
  Result := e.Current.ToString;
  while e.MoveNext do
    Result := Result + separator + e.Current.ToString;
end;

procedure TSpringListAction.DoExecute;
var
  InnerList1: IList<Integer>;
  InnerList2: IList<Integer>;
  OuterList: IList<IEnumerable<Integer>>;
  Concated: IEnumerable<Integer>;
begin
  InnerList1 := TCollections.CreateList<Integer>([1, 2, 3]);
  InnerList2 := TCollections.CreateList<Integer>([4, 5]);
  OuterList := TCollections.CreateList < IEnumerable < Integer >>
    ([InnerList1, InnerList2]);
  Concated := TEnumerable.SelectMany<IEnumerable<Integer>, Integer>(OuterList,
    function(x: IEnumerable<Integer>): IEnumerable<Integer>
    begin
      Result := x;
    end);
  ConsoleWrite('---- Spring4D Lists ----------------------');
  ConsoleWrite('Demo: Spring4D List manipulation, using TSelectManyIterator');
  ConsoleWrite('List1 = [' + JoinEnumerable(',', InnerList1) + ']');
  ConsoleWrite('List2 = [' + JoinEnumerable(',', InnerList2) + ']');
  ConsoleWrite('OuterList = [[' + JoinEnumerable(',', InnerList1) + '],[' +
    JoinEnumerable(',', InnerList2) + ']]');
  ConsoleWrite('ConcatedList = [' + JoinEnumerable(',', Concated) + ']');
end;

end.
