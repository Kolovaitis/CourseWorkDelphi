unit StartMenu;

interface

uses
   System.SysUtils, System.Types, System.UITypes, System.Classes,
   System.Variants,
   FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
   FMX.Controls.Presentation, FMX.StdCtrls, Main, GameRecords, FMX.Menus,
   ResultsForm, FMX.Objects, HelpForm;

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
      procedure ButtonStartClick(Sender: TObject);
      procedure ViewResults();
      procedure MIResultsClick(Sender: TObject);
      procedure ViewHelp();
      procedure MIHelpClick(Sender: TObject);
      procedure StartGame(FieldRows, FieldCols, PlayerCount: Integer);
   private

   public const
      Level: array [0 .. 3, 1 .. 2] of Integer = ((2, 2), (3, 2),
        (2, 4), (3, 4));
   end;

var
   FormStart: TFormStart;
   MainForm: TFormMain;
   ResultsForm: TFormResults;
   HelpForm: TFormHelp;

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

procedure TFormStart.MIHelpClick(Sender: TObject);
begin
   ViewHelp;
end;

procedure TFormStart.MIResultsClick(Sender: TObject);
begin
   ViewResults;
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