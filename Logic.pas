unit Logic;

interface

type
   PCardPointer = ^TCard;

   TCard = record
      EqualCard: PCardPointer;
      IsTurned: Boolean;
      Row: Integer;
      Column: Integer;
      PairNumber: Integer;
   end;

   TMatrix = array of array of TCard;

   TLogic = class
   private
      IsNowTurned: Boolean;
      PastTurnedCard: PCardPointer;
      WinPair: Integer;
      PlayerCount: Integer;
      CurrentPlayer: Integer;
      Points: array of Integer;
      procedure RandomizeCards;
      procedure ToNextPlayer();
      procedure ChooseWinner();
   public
      Field: TMatrix;
      FlipCard: procedure(Row, Column: Integer) of object;
      FlipTwo: procedure(Row1, Column1, Row2, Column2: Integer) of object;
      OnTouch: procedure(Row, Column: Integer) of object;
      WinScenary: procedure(Const Points: array of Integer) of object;
      RefreshPoints: procedure(NewPoints, PlayerNumber: Integer) of object;
      CountUp: Procedure() of object;
      ChangePlayer: Procedure(NewPlayer: Integer) of object;
      procedure OnCardTouched(Row, Column: Integer);
      constructor Create(RowCount, ColumnCount, PlayerCount: Integer);

   end;

implementation

{ TLogic }

procedure TLogic.ChooseWinner;

begin

   WinScenary(Points);
end;

constructor TLogic.Create(RowCount, ColumnCount, PlayerCount: Integer);
var
   i: Integer;
   j: Integer;
begin
   Self.PlayerCount := PlayerCount;
   CurrentPlayer := 0;
   SetLength(Points, PlayerCount);
   SetLength(Field, RowCount, ColumnCount);
   for i := 0 to RowCount - 1 do
      for j := 0 to ColumnCount - 1 do
      begin
         with Field[i, j] do
         begin
            IsTurned := false;
            Row := i;
            Column := j;
         end;
      end;
   RandomizeCards;
   IsNowTurned := false;
   OnTouch := Self.OnCardTouched;
end;

procedure TLogic.OnCardTouched(Row, Column: Integer);
begin
   if (not Field[Row, Column].IsTurned) then
   begin
      if (IsNowTurned) then
      begin
         FlipCard(Row, Column);
         Field[Row, Column].IsTurned := true;
         IsNowTurned := false;
         if (Field[Row, Column].EqualCard <> PastTurnedCard) then
         begin
            // Не совпали
            FlipTwo(Row, Column, PastTurnedCard.Row, PastTurnedCard.Column);
            PastTurnedCard.IsTurned := false;
            Field[Row, Column].IsTurned := false;
            if (PlayerCount > 1) then
               ToNextPlayer;
         end
         else
         Begin
            // Совпали
            Inc(Points[CurrentPlayer]);
            if (PlayerCount > 1) then
               RefreshPoints(Points[CurrentPlayer], CurrentPlayer);
            Dec(WinPair);
            if (WinPair = 0) then
               ChooseWinner;
         End;
      end
      else
      begin
         CountUp;
         IsNowTurned := true;
         Field[Row, Column].IsTurned := true;
         PastTurnedCard := @Field[Row, Column];
         FlipCard(Row, Column);
      end;
   end;

end;

procedure TLogic.RandomizeCards;
var
   i: Integer;
   j: Integer;
   Counter: Integer;
   MaxI: Integer;
   MaxJ: Integer;
   Index2y: Integer;
   Index2x: Integer;
   Index1y: Integer;
   Index1x: Integer;
   TempEq: PCardPointer;
   TempPairNumber: Integer;

begin
   Counter := 0;
   MaxI := High(Field);
   MaxJ := High(Field[0]);
   for i := 0 to MaxI do
   begin
      j := 0;
      while j < MaxJ do
      begin
         Field[i, j].EqualCard := @Field[i, j + 1];
         Field[i, j + 1].EqualCard := @Field[i, j];
         Field[i, j].PairNumber := Counter;
         Field[i, j + 1].PairNumber := Counter;
         Inc(j, 2);
         Inc(Counter);
      end;
   end;
   WinPair := Counter;
   for i := 1 to 2 * MaxI * MaxJ do
   begin
      Index1x := Random(MaxI + 1);
      Index1y := Random(MaxJ + 1);
      Index2x := Random(MaxI + 1);
      Index2y := Random(MaxJ + 1);
      If (Field[Index2x, Index2y].EqualCard <> @Field[Index1x, Index1y]) then
      begin
         TempPairNumber := Field[Index1x, Index1y].PairNumber;
         Field[Index1x, Index1y].PairNumber := Field[Index2x, Index2y]
           .PairNumber;
         Field[Index2x, Index2y].PairNumber := TempPairNumber;
         TempEq := Field[Index1x, Index1y].EqualCard;
         Field[Index1x, Index1y].EqualCard := Field[Index2x, Index2y].EqualCard;
         Field[Index2x, Index2y].EqualCard := TempEq;
         Field[Index1x, Index1y].EqualCard.EqualCard :=
           @Field[Index1x, Index1y];
         Field[Index2x, Index2y].EqualCard.EqualCard :=
           @Field[Index2x, Index2y];
      end;
   end;

end;

procedure TLogic.ToNextPlayer;
begin

   Inc(CurrentPlayer);
   if (CurrentPlayer = PlayerCount) then
      CurrentPlayer := 0;
   ChangePlayer(CurrentPlayer);
end;

end.
