unit WinForm;

interface

uses
   System.SysUtils, System.Types, System.UITypes, System.Classes,
   System.Variants,
   FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
   FMX.Controls.Presentation, FMX.StdCtrls, System.Rtti, FMX.Grid.Style,
   FMX.ScrollBox, FMX.Grid, GameRecords, Parser, FMX.Objects, SoundManager;

type
   TFormWin = class(TForm)
      LCount: TLabel;
      LGoodResult: TLabel;
      LTime: TLabel;
      ImSave: TImage;
      ImBackground: TImage;
      ImNotSave: TImage;
      procedure ImSaveClick(Sender: TObject);
      procedure ImNotSaveClick(Sender: TObject);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);
   private
   var
      TimeSec, TurnCount, ColCount, RowCount: Integer;

   public
      Constructor Create(AOwner: TComponent; const Results: array of Integer;
        Count: Integer; TimeSeconds, ColCount, RowCount: Integer); overload;
      Function SaveResult(): Boolean;
      procedure PrintTime();
      procedure PrintCount();
      procedure PrintWinner(const Results: array of Integer);
   end;

var
   FormWin: TFormWin;

const
   MOV_COUNT = 'Количество ходов: ';
   TIME_STR = 'Время: ';
   INPUTBOX_TITLE = 'Сохранение результата';
   INPUTBOX_TEXT = 'Пожалуйста, укажите своё имя';
   INPUTBOX_ERROR = 'Произошла ошибка сохранения';
   LCOUT_TEXT = 'Победил игрок %d, который набрал %d очков';

implementation

{$R *.fmx}
{ TFormWin }

procedure TFormWin.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   TAudioManager.GetInstance(Self).StopWinSound;
end;

procedure TFormWin.ImNotSaveClick(Sender: TObject);
begin
   Self.Close;
end;

procedure TFormWin.ImSaveClick(Sender: TObject);
begin
   if (SaveResult) then
      Self.Close;
end;

constructor TFormWin.Create(AOwner: TComponent; const Results: array of Integer;
  Count: Integer; TimeSeconds, ColCount, RowCount: Integer);
var
   i: Integer;
   PlayerCol: TColumn;
   PointsCol: TColumn;

begin
   Inherited Create(AOwner);
   Self.ColCount := ColCount;
   Self.RowCount := RowCount;
   TimeSec := TimeSeconds;
   PrintTime;
   if (Length(Results) > 1) then
      PrintWinner(Results)
   else
   begin
      TurnCount := Count;
      PrintCount;
   end;

end;

procedure TFormWin.PrintCount;
begin
   LCount.Text := MOV_COUNT + IntToStr(TurnCount);
end;

procedure TFormWin.PrintTime;
begin

   LTime.Text := TIME_STR + TimeToStr(TimeSec);
end;

procedure TFormWin.PrintWinner(const Results: array of Integer);
var
   Winner: Integer;
   i, WinPoints: Integer;

begin
   LGoodResult.Visible := false;
   ImSave.Visible := false;
   Winner := 0;
   WinPoints := Results[Winner];
   for i := 1 to High(Results) do
      if (Results[i] > WinPoints) then
      begin
         Winner := i;
         WinPoints := Results[Winner];
      end;
   LCount.Text := Format(LCOUT_TEXT, [Winner + 1, WinPoints]);
end;

function TFormWin.SaveResult: Boolean;
var
   ResultValue: TResult;
   Name: String;
begin

   Name := InputBox(INPUTBOX_TITLE, INPUTBOX_TEXT, '');
   if (Name <> '') then
   begin
      result := true;
      ResultValue.TimeSec := TimeSec;
      ResultValue.Name := Name;
      ResultValue.Steps := TurnCount;
      ResultValue.FieldRows := RowCount;
      ResultValue.FieldCols := ColCount;
      if (not TRecordManager.GetInstance.AppendResult(ResultValue)) then
      begin
         ShowMessage(INPUTBOX_ERROR);
         result := false;
      end;
   end
   else
      result := false;
end;

end.
