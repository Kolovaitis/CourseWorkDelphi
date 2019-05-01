﻿unit MyCardView;

interface

uses
   System.SysUtils, System.Classes, FMX.Types, FMX.Controls3D, FMX.Objects3D,
   FMX.Ani, FMX.MaterialSources;

type
   TCardView = class(TRectangle3D)
   private
   var
      Row, Column: Integer;
      Timer: TTimer;
      Animation: TFloatAnimation;

   Const
      DeckX = 2;
      DeckY = 3;
      ANGLE = 180;

      StartX = 0;
      StartY = 0;
      PropertyName = 'RotationAngle.Y';
      Duration = 0.2;
      procedure HasClicked(Sender: TObject);
      procedure ForTimer(Sender: TObject);
   protected
      { Protected declarations }
   public const
      StandartWidth = 3;
      StandartHeight = 5;
      DeltaX = 3.5;
      DeltaY = 5.5;
      Constructor CreateCard(AOwner: TFMXObject;
        const MaterialSourceBack, MaterialSourceFront, MaterialSourceShaft
        : TMaterialSource; Row, Column: Integer);
      Constructor Create(AOwner: TComponent); override;
      procedure Flip();
      procedure FlipAfterSecond(Time: Cardinal);
      procedure CreateAndPlayStartAnimation(Row, Column: Integer);

   var
      OnHasClick: procedure(i, j: Integer) of object;

   published

      { Published declarations }
   end;

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents('Samples', [TCardView]);
end;

{ TCardView }

constructor TCardView.Create(AOwner: TComponent);
begin
   inherited;

end;

procedure TCardView.CreateAndPlayStartAnimation(Row, Column: Integer);
var
   StartAnimationX, StartAnimationY: TFloatAnimation;
   StartAnimationZ: TFloatAnimation;
begin
   StartAnimationX := TFloatAnimation.Create(Self);
   StartAnimationX.Parent := Self;
   StartAnimationX.PropertyName := 'Position.X';
   StartAnimationX.Duration := 1;
   StartAnimationX.Enabled := true;
   StartAnimationX.StopValue := StartX + DeltaX * Column;
   StartAnimationX.StartValue := 10 - Random(20);
   StartAnimationX.Start;
   StartAnimationY := TFloatAnimation.Create(Self);
   StartAnimationY.Parent := Self;
   StartAnimationY.PropertyName := 'Position.Y';
   StartAnimationY.Duration := 1;
   StartAnimationY.Enabled := true;
   StartAnimationY.StopValue := StartY + DeltaY * Row;
   StartAnimationY.StartValue := Random(30);
   StartAnimationY.Start;

   // Self.Position.X := StartX + DeltaX * Column;
   // Self.Position.Y := StartY + DeltaY * Row;
end;

constructor TCardView.CreateCard(AOwner: TFMXObject;
  const MaterialSourceBack, MaterialSourceFront, MaterialSourceShaft
  : TMaterialSource; Row, Column: Integer);
begin
   Self := TCardView.Create(AOwner);

   Self.Row := Row;
   Self.Column := Column;
   Animation := TFloatAnimation.Create(Self);
   Animation.Parent := Self;
   Animation.PropertyName := PropertyName;
   Animation.Duration := Duration;
   Animation.Enabled := true;
   Self.Flatness := 1;
   Self.Height := StandartHeight;
   Self.Width := StandartWidth;
   Self.Depth := 0.05;
   Self.Parent := AOwner;
   Self.CreateAndPlayStartAnimation(Row, Column);
   Self.MaterialBackSource := MaterialSourceBack;
   Self.MaterialSource := MaterialSourceFront;
   Self.MaterialShaftSource := MaterialSourceShaft;
   Self.OnClick := HasClicked;
   Self.RotationAngle.Y := ANGLE;
   Animation.StopValue := ANGLE;
   Timer := TTimer.Create(AOwner);
   Timer.Enabled := false;
   Timer.OnTimer := ForTimer;
end;

procedure TCardView.Flip;
begin
   Animation.StartValue := Animation.StopValue;
   Animation.StopValue := Animation.StartValue + ANGLE;
   Animation.Start;

end;

procedure TCardView.FlipAfterSecond;
begin
   Timer.Interval := Time;
   Timer.Enabled := true;
end;

procedure TCardView.ForTimer(Sender: TObject);
begin
   Timer.Enabled := false;
   Self.Flip;
end;

procedure TCardView.HasClicked(Sender: TObject);
begin
   OnHasClick(Self.Row, Self.Column);
end;

end.
