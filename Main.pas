﻿unit Main;

interface

uses
   System.SysUtils, System.Types, System.UITypes, System.Classes,
   System.Variants,
   FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
   FMX.Dialogs, System.Math.Vectors, FMX.Controls3D, FMX.Objects3D,
   FMX.MaterialSources, FMX.Media, FMX.Controls.Presentation, FMX.StdCtrls,
   Logic,
   FMX.Ani, MyCardView, FMX.Layers3D, WinForm, FMX.Menus, Parser, SoundManager;

type
   TCords = Record
      I: Integer;
      J: Integer;
   End;

   TControlMatrix = array of array of TCardView;

   TFormMain = class(TForm3D)
      Material1: TTextureMaterialSource;
      TextureMaterialSourceBack: TTextureMaterialSource;
      CameraMain: TCamera;
      DummyAllCards: TDummy;
      Material2: TTextureMaterialSource;
      Material3: TTextureMaterialSource;
      Material4: TTextureMaterialSource;
      Material5: TTextureMaterialSource;
      Material6: TTextureMaterialSource;
      MaterialShaft: TColorMaterialSource;
      DummyMainScene: TDummy;
      TimerSinglePlayer: TTimer;
      MenuCount: TMenuItem;
      MenuTime: TMenuItem;
      MMState: TMainMenu;
      MenuWhoTurn: TMenuItem;

      procedure DummyClick(Sender: TObject);
      procedure Form3DShow(Sender: TObject);
      procedure TimerSinglePlayerTimer(Sender: TObject);
   private
      AudioManager:TAudioManager;
      AllCards: TControlMatrix;
      Logic: TLogic;
      Rows, Cols: Integer;
      PlayerCount: Integer;
      TimeSeconds: Integer;
      Count: Integer;
      PointsMenu: array of TMenuItem;
      function GetCords(const Sender: TRectangle3D): TCords;

      procedure FlipTwo(Row1, Column1, Row2, Column2: Integer);
      procedure FlipCard(Row, Column: Integer);
      procedure Win(const Points: Array of Integer);
      procedure RefreshPoints(NewPoints, Player: Integer);
      procedure InitMaterials();
      procedure InitLogic();
      procedure SetScale();
      procedure CountUp();
      procedure ChangePlayer(NewPlayer: Integer);

   public
      Materials: Array of TMaterialSource;
      Constructor Create(AOwner: TComponent; Rows, Columns: Integer;
        PlayerCount: Integer); overload;
   end;

var
   FormMain: TFormMain;

const
   TimeToSee = 1000;
   WHO_TURN = 'Ходит игрок %d';
   MOV_COUNT = 'Количество попыток: %d';
   START_POINTS_MENU = 'Игрок %d: 0 ';
   POINTS_MENU = 'Игрок %d: %d ';
   TIME_STR = 'Время: ';

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

procedure TFormMain.FlipCard(Row, Column: Integer);
begin
   AudioManager.PlayCardTurnSound;
   Self.AllCards[Row, Column].Flip;
end;

procedure TFormMain.FlipTwo(Row1, Column1, Row2, Column2: Integer);
begin
   AudioManager.PlayCardTurnSound;
   AllCards[Row1, Column1].FlipAfterSecond(TimeToSee);
   AllCards[Row2, Column2].FlipAfterSecond(TimeToSee);
end;

procedure TFormMain.ChangePlayer(NewPlayer: Integer);
begin
   MenuWhoTurn.Text := Format(WHO_TURN, [NewPlayer + 1]);
end;

procedure TFormMain.CountUp;
begin
   Inc(Count);
   MenuCount.Text := Format(MOV_COUNT, [Count]);
end;

constructor TFormMain.Create(AOwner: TComponent; Rows, Columns: Integer;
  PlayerCount: Integer);
begin
   inherited Create(AOwner);
   Self.Cols := Columns;
   Self.Rows := Rows;
   Self.PlayerCount := PlayerCount;
end;

procedure TFormMain.DummyClick(Sender: TObject);
var
   Cords: TCords;
begin
   Cords := GetCords(Sender as TRectangle3D);
   Logic.OnCardTouched(Cords.I, Cords.J);
end;

procedure TFormMain.Form3DShow(Sender: TObject);
var
   I: Integer;
begin
AudioManager:=TAudioManager.GetInstance(Self);
   if (PlayerCount > 1) then
   begin
      SetLength(PointsMenu, PlayerCount);
      for I := 0 to PlayerCount - 1 do
      begin
         PointsMenu[I] := TMenuItem.Create(Self);
         PointsMenu[I].Parent := MMState;
         PointsMenu[I].Text := Format(START_POINTS_MENU, [I + 1]);
      end;
      MenuWhoTurn.Visible := true;
   end;
   TimeSeconds := 0;
   TimerSinglePlayer.Enabled := true;

   SetScale;
   InitMaterials;
   InitLogic;
end;

function TFormMain.GetCords(const Sender: TRectangle3D): TCords;
var
   Answer: TCords;
   I, J: Integer;
begin
   for I := 0 to High(AllCards) do
      for J := 0 to High(AllCards) do
      begin
         if (AllCards[I, J] = Sender) then
         begin
            Answer.I := I;
            Answer.J := J;
         end;
      end;
   GetCords := Answer;
end;

procedure TFormMain.InitLogic;
var
   I: Integer;
   J: Integer;
begin
   Count := 0;
   SetLength(AllCards, Rows, Cols);
   Logic := TLogic.Create(Rows, Cols, PlayerCount);
   Logic.FlipCard := Self.FlipCard;
   Logic.FlipTwo := Self.FlipTwo;
   Logic.WinScenary := Self.Win;
   if (PlayerCount > 1) then
   begin
      Logic.RefreshPoints := Self.RefreshPoints;
      Logic.ChangePlayer := Self.ChangePlayer;
   end;
   Logic.CountUp := Self.CountUp;
   for I := 0 to Rows - 1 do
      for J := 0 to Cols - 1 do
      begin

         AllCards[I, J] := TCardView.CreateCard(DummyAllCards,
           TextureMaterialSourceBack, Materials[Logic.Field[I, J].PairNumber],
           MaterialShaft, I, J);
         AllCards[I, J].OnHasClick := Logic.OnCardTouched;
      end;
end;

procedure TFormMain.InitMaterials;
begin
   SetLength(Materials, 6);
   Materials[0] := Material1;
   Materials[1] := Material2;
   Materials[2] := Material3;
   Materials[3] := Material4;
   Materials[4] := Material5;
   Materials[5] := Material6;
end;

procedure TFormMain.RefreshPoints(NewPoints, Player: Integer);
begin
   PointsMenu[Player].Text := Format(POINTS_MENU, [Player + 1, NewPoints]);
end;

procedure TFormMain.SetScale;
begin
   DummyAllCards.Position.X := (TCardView.StandartWidth - TCardView.DeltaX *
     Cols) / 2;
   DummyAllCards.Position.Y := (TCardView.StandartHeight - TCardView.DeltaY *
     Rows) / 2;
   CameraMain.AngleOfView := 15 * (Rows + 1);

end;

procedure TFormMain.TimerSinglePlayerTimer(Sender: TObject);

begin
   Inc(TimeSeconds);

   MenuTime.Text := TIME_STR + TimeToStr(TimeSeconds);
end;

procedure TFormMain.Win(const Points: Array of Integer);
var
   WinForm: TFormWin;
begin
   TimerSinglePlayer.Enabled := false;
   TAudioManager.GetInstance(Self).PlayWinSound;
   WinForm := TFormWin.Create(Self, Points, Count, TimeSeconds, Cols, Rows);
   WinForm.ShowModal();

   Self.Close();

end;

end.
