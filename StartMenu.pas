unit StartMenu;

interface

uses
   System.SysUtils, System.Types, System.UITypes, System.Classes,
   System.Variants,
   FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
   FMX.Controls.Presentation, FMX.StdCtrls, Main, GameRecords, FMX.Menus,
   ResultsForm, FMX.Objects, HelpForm, SoundManager;

type
   TFormStart = class(TForm)
      ComboBoxLevelSelection: TComboBox;
      ButtonStart: TButton;
      MIResults: TMenuItem;
      MBMain: TMenuBar;
      MIHelp: TMenuItem;
      ImBackground: TImage;
      RoundRectMain: TRoundRect;
      LField: TLabel;
      LPlayerCount: TLabel;
      ComboBoxPlayerCount: TComboBox;
      ImSound: TImage;
      procedure ButtonStartClick(Sender: TObject);

      procedure MIResultsClick(Sender: TObject);

      procedure MIHelpClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure ImSoundClick(Sender: TObject);

   private
      BitmapForSound: array [false .. true] of TBitmap;
      procedure StartGame(FieldRows, FieldCols, PlayerCount: Integer);
      procedure ViewHelp();
      procedure ViewResults();
      procedure InitSoundOffBmp();
      procedure SoundModeChange();
      procedure ChangeSoundImage(SoundImage: Boolean);
   public const
      Level: array [0 .. 3, 1 .. 2] of Integer = ((2, 2), (3, 2),
        (2, 4), (3, 4));
      SoundBitmap: array [false .. true] of String = ('SoundOffImage',
        'SoundOnImage');
   end;

var
   FormStart: TFormStart;
   MainForm: TFormMain;
   ResultsForm: TFormResults;
   HelpForm: TFormHelp;
   AudioManager: TAudioManager;

implementation

{$R *.fmx}

procedure TFormStart.ButtonStartClick(Sender: TObject);

var

   Index: Integer;
begin
   Index := ComboBoxLevelSelection.ItemIndex;
   StartGame(Level[Index, 1], Level[Index, 2],
     ComboBoxPlayerCount.ItemIndex + 1);
end;

procedure TFormStart.ChangeSoundImage(SoundImage: Boolean);
begin
   ImSound.Bitmap := BitmapForSound[SoundImage];
end;

procedure TFormStart.FormShow(Sender: TObject);

begin
   InitSoundOffBmp;
   AudioManager := TAudioManager.GetInstance(Self);
   AudioManager.ChangeSoundImage := ChangeSoundImage;
end;

procedure TFormStart.ImSoundClick(Sender: TObject);
begin

   SoundModeChange;
end;

procedure TFormStart.InitSoundOffBmp;
var
   InStream: TResourceStream;
   i: Boolean;
begin
   for i := false to true do
   begin
      InStream := TResourceStream.Create(HInstance, SoundBitmap[i],
        PChar(RT_RCDATA));
      try
         BitmapForSound[i] := TBitmap.Create;
         BitmapForSound[i].LoadFromStream(InStream);
      finally
         InStream.Free;
      end;
   end;
end;

procedure TFormStart.MIHelpClick(Sender: TObject);
begin
   ViewHelp;
end;

procedure TFormStart.MIResultsClick(Sender: TObject);
begin
   ViewResults;
end;

procedure TFormStart.SoundModeChange;
var
   TempBmp: TBitmap;
begin
   AudioManager.ChangeSoundState;
end;

procedure TFormStart.StartGame(FieldRows, FieldCols, PlayerCount: Integer);
begin
   Index := ComboBoxLevelSelection.ItemIndex;
   if (MainForm <> nil) then
      MainForm.Destroy;
   MainForm := TFormMain.Create(Self, FieldRows, FieldCols, PlayerCount);
   MainForm.ShowModal;
end;

procedure TFormStart.ViewHelp;
begin
   if (HelpForm = nil) then
      HelpForm := TFormHelp.Create(Self);
   HelpForm.ShowModal;
end;

procedure TFormStart.ViewResults;
begin

   if (ResultsForm <> nil) then
      ResultsForm.Destroy;
   ResultsForm := TFormResults.Create(Self);
   ResultsForm.ShowModal;
end;

end.
