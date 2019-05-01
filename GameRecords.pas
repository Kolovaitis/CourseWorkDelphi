unit GameRecords;

interface

uses
   System.SysUtils;

type
   TResult = Record
      TimeSec: Integer;
      Name: ShortString;
      FieldRows: Integer;
      FieldCols: Integer;
      Steps: Integer;
   End;

   TDataType = array of ^TResult;

   TRecordManager = class(TObject)
   private
      class var Instance: TRecordManager;
      constructor Create();
      function GetData(): TDataType;

   var
      CurrentData: TDataType;
      WorkingFile: File of TResult;

   const
      FilePath = 'data.nik';
   public
      class function GetInstance(): TRecordManager;
      property Data: TDataType read GetData;
      destructor Destroy; override;
      function AppendResult(const Value: TResult): Boolean; // true, если все ок

   end;

var
   WorkingFile: File of TResult;

implementation

function TRecordManager.AppendResult(const Value: TResult): Boolean;
begin
   try
      Reset(WorkingFile);
      Seek(WorkingFile, FileSize(WorkingFile));
      Write(WorkingFile, Value);
      Result := true;
   except
      Result := false;
   end;
end;

constructor TRecordManager.Create;
begin
   AssignFile(WorkingFile, FilePath);
   if (not FileExists(FilePath)) then
   begin
      // FileCreate(FilePath);
      Rewrite(WorkingFile);
   end;

end;

destructor TRecordManager.Destroy;
begin
   inherited;
   if (Instance <> nil) then
      Instance.Free;
   CloseFile(WorkingFile);
end;

function TRecordManager.GetData: TDataType;
var
   Data: TDataType;
   EachResult: TResult;
   Size, i: Integer;
begin
   try
      Reset(WorkingFile);
      Size := FileSize(WorkingFile);
      SetLength(Data, Size);

      for i := 0 to Size - 1 do
      begin
         New(Data[i]);
         Read(WorkingFile, Data[i]^);

      end;

   finally
      GetData := Data;
   end;
end;

class function TRecordManager.GetInstance: TRecordManager;
begin
   if (Instance = nil) then
      Instance := TRecordManager.Create;
   GetInstance := Instance;
end;

end.
