unit ResultsForm;

interface

uses
   System.SysUtils, System.Types, System.UITypes, System.Classes,
   System.Variants,
   FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
   FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid,
   GameRecords, Parser, FMX.Objects;

const
   MaxColumnIndex = 3;
   NAME_STR = 'Имя';
   TIME_STR = 'Время';
   COUNT_STR = 'Количество попыток';
   FIELD_SIZE_STR = 'Размер поля';
   STANDART_COLUMN_WIDTH = 100;
   COUNT_COLUMN_WIDTH = 125;

type
   PRowData = ^TRowData;
   TRowData = array [0 .. MaxColumnIndex] of String;

   TFormResults = class(TForm)
      GrTable: TGrid;
      procedure FormShow(Sender: TObject);
      procedure GrTableGetValue(Sender: TObject; const ACol, ARow: Integer;
        var Value: TValue);
      procedure GrTableHeaderClick(Column: TColumn);
   private
      A: array of TValue;
      DataRecords: array of PRowData;
      procedure SortColumn(ColumnToSort: Integer);
      function CompareStringsLocal(Str1, Str2: String;
        ColIndex: Integer): Boolean;
   public
      ColumnName, ColumnTime, ColumnCount, ColumnField: TColumn;
      DataRowCount: Integer;
   end;

var
   FormResults: TFormResults;

implementation

{$R *.fmx}

function TFormResults.CompareStringsLocal(Str1, Str2: String;
  ColIndex: Integer): Boolean;
begin
   if (ColIndex <> 2) then
      Result := CompareStrings(Str1, Str2)
   else
      Result := StrToInt(Str2) < StrToInt(Str1);
end;

procedure TFormResults.FormShow(Sender: TObject);
var
   Data: TDataType;
   i: Integer;
begin
   ColumnName := TColumn.Create(Self);
   ColumnName.Parent := GrTable;
   ColumnName.Header := NAME_STR;
   ColumnName.Width := STANDART_COLUMN_WIDTH;

   ColumnTime := TColumn.Create(Self);
   ColumnTime.Parent := GrTable;
   ColumnTime.Header := TIME_STR;
   ColumnTime.Width := STANDART_COLUMN_WIDTH;

   ColumnCount := TColumn.Create(Self);
   ColumnCount.Parent := GrTable;
   ColumnCount.Header := COUNT_STR;
   ColumnCount.Width := COUNT_COLUMN_WIDTH;

   ColumnField := TColumn.Create(Self);
   ColumnField.Parent := GrTable;
   ColumnField.Header := FIELD_SIZE_STR;
   ColumnField.Width := STANDART_COLUMN_WIDTH;
   Data := TRecordManager.GetInstance.Data;
   DataRowCount := Length(Data);
   SetLength(DataRecords, DataRowCount);
   for i := 0 to High(Data) do
   begin
      with Data[i]^ do
      begin
         New(DataRecords[i]);
         DataRecords[i][0] := Name;
         DataRecords[i][1] := TimeToStr(TimeSec);
         DataRecords[i][2] := IntToStr(Steps);
         DataRecords[i][3] := FieldToStr(FieldCols, FieldRows);
      end;
   end;
   GrTable.RowCount := DataRowCount;
end;

procedure TFormResults.GrTableGetValue(Sender: TObject;
  const ACol, ARow: Integer; var Value: TValue);
begin
   Value := DataRecords[ARow][ACol];

end;

procedure TFormResults.GrTableHeaderClick(Column: TColumn);
begin
   GrTable.BeginUpdate;
   SortColumn(Column.Index);
   GrTable.EndUpdate;

end;

procedure TFormResults.SortColumn(ColumnToSort: Integer);
var
   i, j: Integer;
   BufRow: PRowData;
begin
   for i := 2 to DataRowCount do
      for j := 0 to DataRowCount - i do

         if (CompareStringsLocal(DataRecords[j][ColumnToSort],
           DataRecords[j + 1][ColumnToSort], ColumnToSort)) then
         begin
            BufRow := DataRecords[j];
            DataRecords[j] := DataRecords[j + 1];
            DataRecords[j + 1] := BufRow;
         end;
end;

end.
