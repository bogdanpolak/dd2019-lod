unit Action.Import.ReadersReport;

interface

uses
  System.Classes, System.JSON, System.SysUtils, System.Generics.Collections,
  VCL.ExtCtrls, VCL.DBGrids,
  Data.Main,
  ChromeTabs, ChromeTabsClasses,
  Action.Base,
  ExtGUI.ListBox.Books;

type
  TImportReadersReportAction = class(TBaseAction)
  private
    FBookListsComponent: TBooksListBoxConfigurator;
    FFramePanel: TPanel;
    FChromeTabs1: TChromeTabs;
    FMainDataModule: TDataModMain;
    procedure Guard;
  public
    procedure DoInitialiaze; override;
    procedure DoExecute; override;
    property BookListsComponent: TBooksListBoxConfigurator
      read FBookListsComponent write FBookListsComponent;
    property FramePanel: TPanel read FFramePanel write FFramePanel;
    property ChromeTabs: TChromeTabs read FChromeTabs1 write FChromeTabs1;
    property MainDataModule: TDataModMain read FMainDataModule
      write FMainDataModule;
  end;

function AutoSizeColumns(DBGrid: TDBGrid; const MaxRows: Integer = 25): Integer;

const
  Client_API_Token = '20be805d-9cea27e2-a588efc5-1fceb84d-9fb4b67c';

implementation

uses
  System.Math, System.DateUtils, System.RegularExpressions, System.Variants,
  VCL.Controls, VCL.Forms,
  Data.DB,
  Frame.Import,
  ClientAPI.Books,
  ClientAPI.Readers,
  Helper.TApplication;

{ TODO 2: [Helper] Extract into TDBGrid.ForEachRow class helper }
function AutoSizeColumns(DBGrid: TDBGrid; const MaxRows: Integer = 25): Integer;
var
  DataSet: TDataSet;
  Bookmark: TBookmark;
  Count, i: Integer;
  ColumnsWidth: array of Integer;
begin
  SetLength(ColumnsWidth, DBGrid.Columns.Count);
  for i := 0 to DBGrid.Columns.Count - 1 do
    if DBGrid.Columns[i].Visible then
      ColumnsWidth[i] := DBGrid.Canvas.TextWidth
        (DBGrid.Columns[i].title.Caption + '   ')
    else
      ColumnsWidth[i] := 0;
  if DBGrid.DataSource <> nil then
    DataSet := DBGrid.DataSource.DataSet
  else
    DataSet := nil;
  if (DataSet <> nil) and DataSet.Active then
  begin
    Bookmark := DataSet.GetBookmark;
    DataSet.DisableControls;
    try
      Count := 0;
      DataSet.First;
      while not DataSet.Eof and (Count < MaxRows) do
      begin
        for i := 0 to DBGrid.Columns.Count - 1 do
          if DBGrid.Columns[i].Visible then
            ColumnsWidth[i] := System.Math.Max(ColumnsWidth[i],
              DBGrid.Canvas.TextWidth(DBGrid.Columns[i].Field.Text + '   '));
        Inc(Count);
        DataSet.Next;
      end;
    finally
      DataSet.GotoBookmark(Bookmark);
      DataSet.FreeBookmark(Bookmark);
      DataSet.EnableControls;
    end;
  end;
  Count := 0;
  for i := 0 to DBGrid.Columns.Count - 1 do
    if DBGrid.Columns[i].Visible then
    begin
      DBGrid.Columns[i].Width := ColumnsWidth[i];
      Inc(Count, ColumnsWidth[i]);
    end;
  Result := Count - DBGrid.ClientWidth;
end;

function BooksToDateTime(const s: string): TDateTime;
const
  months: array [1 .. 12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  m: string;
  y: string;
  i: Integer;
  mm: Integer;
  yy: Integer;
begin
  m := s.Substring(0, 3);
  y := s.Substring(4);
  mm := 0;
  for i := 1 to 12 do
    if months[i].ToUpper = m.ToUpper then
      mm := i;
  if mm = 0 then
    raise ERangeError.Create('Incorect mont name in the date: ' + s);
  yy := y.ToInteger();
  Result := EncodeDate(yy, mm, 1);
end;

// ----------------------------------------------------------
//
// Function checks is TJsonObject has field and this field has not null value
//
{ TODO 2: [Helper] TJSONObject Class helpper and more minigful name expected }
function fieldAvaliable(jsObject: TJSONObject; const fieldName: string)
  : boolean; inline;
begin
  Result := Assigned(jsObject.Values[fieldName]) and not jsObject.Values
    [fieldName].Null;
end;

{ TODO 2: [Helper] TJSONObject Class helpper and this method has two responsibilities }
// Warning! In-out var parameter
// extract separate:  GetIsoDateUtc
function IsValidIsoDateUtc(jsObj: TJSONObject; const Field: string;
  var dt: TDateTime): boolean;
begin
  dt := 0;
  try
    dt := System.DateUtils.ISO8601ToDate(jsObj.Values[Field].Value, False);
    Result := True;
  except
    on E: Exception do
      Result := False;
  end
end;

{ TODO 2: Move into Utils.General }
function CheckEmail(const s: string): boolean;
const
  EMAIL_REGEX = '^((?>[a-zA-Z\d!#$%&''*+\-/=?^_`{|}~]+\x20*|"((?=[\x01-\x7f])' +
    '[^"\\]|\\[\x01-\x7f])*"\x20*)*(?<angle><))?((?!\.)' +
    '(?>\.?[a-zA-Z\d!#$%&''*+\-/=?^_`{|}~]+)+|"((?=[\x01-\x7f])' +
    '[^"\\]|\\[\x01-\x7f])*")@(((?!-)[a-zA-Z\d\-]+(?<!-)\.)+[a-zA-Z]' +
    '{2,}|\[(((?(?<!\[)\.)(25[0-5]|2[0-4]\d|[01]?\d?\d))' +
    '{4}|[a-zA-Z\d\-]*[a-zA-Z\d]:((?=[\x01-\x7f])[^\\\[\]]|\\' +
    '[\x01-\x7f])+)\])(?(angle)>)$';
begin
  Result := System.RegularExpressions.TRegEx.IsMatch(s, EMAIL_REGEX);
end;

// TODO 3: Move this procedure into class (idea)
procedure ValidateReadersReport(jsRow: TJSONObject; email: string;
  var dtReported: TDateTime);
begin
  if not CheckEmail(email) then
    raise Exception.Create('Invalid email addres');
  if not IsValidIsoDateUtc(jsRow, 'created', dtReported) then
    raise Exception.Create('Invalid date. Expected ISO format');
end;

procedure TImportReadersReportAction.DoInitialiaze;
begin
  Caption := 'Import Reader Reports';
end;

procedure TImportReadersReportAction.DoExecute;
var
  NewFrame: TFrameImport;
  tab: TChromeTab;
  jsData: TJSONArray;
  DBGrid1: TDBGrid;
  DataSrc1: TDataSource;
  DBGrid2: TDBGrid;
  DataSrc2: TDataSource;
  i: Integer;
  jsRow: TJSONObject;
  email: string;
  firstName: string;
  lastName: string;
  company: string;
  bookISBN: string;
  bookTitle: string;
  rating: Integer;
  oppinion: string;
  ss: array of string;
  v: string;
  dtReported: TDateTime;
  readerId: Variant;
  b: TBook;
  jsBooks: TJSONArray;
  jsBook: TJSONObject;
  TextBookReleseDate: string;
  b2: TBook;
begin
  Guard;
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  //
  // Import new Books data from OpenAPI
  //
  { TODO 2: [A] Extract method. Read comments and use meaningful name }
  jsBooks := ImportBooksFromWebService(Client_API_Token);
  try
    for i := 0 to jsBooks.Count - 1 do
    begin
      jsBook := jsBooks.Items[i] as TJSONObject;
      b := TBook.Create;
      b.status := jsBook.Values['status'].Value;
      b.title := jsBook.Values['title'].Value;
      b.isbn := jsBook.Values['isbn'].Value;
      b.author := jsBook.Values['author'].Value;
      TextBookReleseDate := jsBook.Values['date'].Value;
      b.releseDate := BooksToDateTime(TextBookReleseDate);
      b.pages := (jsBook.Values['pages'] as TJSONNumber).AsInt;
      b.price := StrToCurr(jsBook.Values['price'].Value);
      b.currency := jsBook.Values['currency'].Value;
      b.description := jsBook.Values['description'].Value;
      b.imported := Now();
      b2 := BookListsComponent.GetBookList(blkAll).FindByISBN(b.isbn);
      if not Assigned(b2) then
      begin
        BookListsComponent.InsertNewBook(b);
        // ----------------------------------------------------------------
        // Append report into the database:
        // Fields: ISBN, Title, Authors, Status, ReleseDate, Pages, Price,
        // Currency, Imported, Description
        MainDataModule.mtabBooks.InsertRecord([b.isbn, b.title, b.author,
          b.status, b.releseDate, b.pages, b.price, b.currency, b.imported,
          b.description]);
      end;
    end;
  finally
    jsBooks.Free;
  end;
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  //
  // Create new frame, show it add to ChromeTabs
  // 1. Create TFrameImport.
  // 2. Embed frame in pnMain (show)
  // 3. Add new ChromeTab
  //
  { TODO 2: [B] Extract method. Read comments and use meaningful }
  // Look for ChromeTabs1.Tabs.Add for code duplication
  if (FramePanel <> nil) and (ChromeTabs <> nil) then
  begin
    NewFrame := TFrameImport.Create(FramePanel);
    NewFrame.Parent := FramePanel;
    NewFrame.Visible := True;
    NewFrame.Align := VCL.Controls.alClient;
    tab := ChromeTabs.Tabs.Add;
    tab.Caption := 'Readers';
    tab.Data := NewFrame;
  end
  else
    NewFrame := nil;
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  //
  // Dynamically Add TDBGrid to TFrameImport
  //
  { TODO 2: [C] Move code down separate bussines logic from GUI }
  // warning for dataset dependencies, discuss TDBGrid dependencies
  if NewFrame <> nil then
  begin
    DataSrc1 := TDataSource.Create(NewFrame);
    DBGrid1 := TDBGrid.Create(NewFrame);
    DBGrid1.AlignWithMargins := True;
    DBGrid1.Parent := NewFrame;
    DBGrid1.Align := VCL.Controls.alClient;
    DBGrid1.DataSource := DataSrc1;
    DataSrc1.DataSet := MainDataModule.mtabReaders;
    AutoSizeColumns(DBGrid1);
  end
  else
    DBGrid1 := nil;
  // ----------------------------------------------------------
  // ----------------------------------------------------------
  //
  // Import new Reader Reports data from OpenAPI
  // - Load JSON from WebService
  // - Validate JSON and insert new a Readers into the Database
  //
  jsData := ImportReaderReportsFromWebService(Client_API_Token);
  { TODO 2: [D] Extract method. Block try-catch is separate responsibility }
  try
    for i := 0 to jsData.Count - 1 do
    begin
      { TODO 3: [A] Extract Reader Report code into the record TReaderReport (model layer) }
      { TODO 2: [F] Repeated code. Violation of the DRY rule }
      // Use TJSONObject helper Values return Variant.Null
      // ----------------------------------------------------------------
      //
      // Read JSON object
      //
      { TODO 3: [A] Move this code into record TReaderReport.LoadFromJSON }
      jsRow := jsData.Items[i] as TJSONObject;
      email := jsRow.Values['email'].Value;
      if fieldAvaliable(jsRow, 'firstname') then
        firstName := jsRow.Values['firstname'].Value
      else
        firstName := '';
      if fieldAvaliable(jsRow, 'lastname') then
        lastName := jsRow.Values['lastname'].Value
      else
        lastName := '';
      if fieldAvaliable(jsRow, 'company') then
        company := jsRow.Values['company'].Value
      else
        company := '';
      if fieldAvaliable(jsRow, 'book-isbn') then
        bookISBN := jsRow.Values['book-isbn'].Value
      else
        bookISBN := '';
      if fieldAvaliable(jsRow, 'book-title') then
        bookTitle := jsRow.Values['book-title'].Value
      else
        bookTitle := '';
      if fieldAvaliable(jsRow, 'rating') then
        rating := (jsRow.Values['rating'] as TJSONNumber).AsInt
      else
        rating := -1;
      if fieldAvaliable(jsRow, 'oppinion') then
        oppinion := jsRow.Values['oppinion'].Value
      else
        oppinion := '';
      // ----------------------------------------------------------------
      //
      // Validate imported Reader report
      //
      { TODO 2: [E] Move validation up. Before reading data }
      ValidateReadersReport(jsRow, email, dtReported);
      // ----------------------------------------------------------------
      //
      // Locate book by ISBN
      //
      { TODO 2: [G] Extract method }
      b := BookListsComponent.GetBookList(blkAll).FindByISBN(bookISBN);
      if not Assigned(b) then
        raise Exception.Create('Invalid book isbn');
      // ----------------------------------------------------------------
      // Find the Reader in then database using an email address
      readerId := MainDataModule.FindReaderByEmil(email);
      // ----------------------------------------------------------------
      //
      // Append a new reader into the database if requred:
      if System.Variants.VarIsNull(readerId) then
      begin
        { TODO 2: [G] Extract method }
        readerId := MainDataModule.GetMaxValueInDataSet
          (MainDataModule.mtabReaders, 'ReaderId') + 1;
        //
        // Fields: ReaderId, FirstName, LastName, Email, Company, BooksRead,
        // LastReport, ReadersCreated
        //
        MainDataModule.mtabReaders.AppendRecord([readerId, firstName, lastName,
          email, company, 1, dtReported, Now()]);
      end;
      // ----------------------------------------------------------------
      //
      // Append report into the database:
      // Fields: ReaderId, ISBN, Rating, Oppinion, Reported
      //
      MainDataModule.mtabReports.AppendRecord([readerId, bookISBN, rating,
        oppinion, dtReported]);
      // ----------------------------------------------------------------
      if Application.IsDeveloperMode then
        Insert([rating.ToString], ss, maxInt);
    end;
    // ----------------------------------------------------------------
    if Application.IsDeveloperMode then
      Caption := String.Join(' ,', ss);
    // ----------------------------------------------------------------
    if (NewFrame <> nil) and (DBGrid1 <> nil) then
    begin
      with TSplitter.Create(NewFrame) do
      begin
        Align := alBottom;
        Parent := NewFrame;
        Height := 5;
      end;
      DBGrid1.Margins.Bottom := 0;
      DataSrc2 := TDataSource.Create(NewFrame);
      DBGrid2 := TDBGrid.Create(NewFrame);
      DBGrid2.AlignWithMargins := True;
      DBGrid2.Parent := NewFrame;
      DBGrid2.Align := alBottom;
      DBGrid2.Height := NewFrame.Height div 3;
      DBGrid2.DataSource := DataSrc2;
      DataSrc2.DataSet := MainDataModule.mtabReports;
      DBGrid2.Margins.Top := 0;
      AutoSizeColumns(DBGrid2);
    end;
  finally
    jsData.Free;
  end;
end;

procedure TImportReadersReportAction.Guard;
begin
  if (BookListsComponent = nil) then
    raise Exception.Create('Error Message');
  if (MainDataModule = nil) then
    raise Exception.Create('Error Message');
end;

end.
