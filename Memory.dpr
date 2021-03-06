program Memory;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {FormMain},
  Logic in 'Logic.pas',
  MyCardView in 'MyCardView.pas',
  StartMenu in 'StartMenu.pas' {FormStart},
  WinForm in 'WinForm.pas' {FormWin},
  GameRecords in 'GameRecords.pas',
  ResultsForm in 'ResultsForm.pas' {FormResults},
  Parser in 'Parser.pas',
  HelpForm in 'HelpForm.pas' {FormHelp},
  SoundManager in 'SoundManager.pas';

{$R *.res}

begin
   Application.Initialize;
   Application.CreateForm(TFormStart, FormStart);
  // Application.CreateForm(TForm1, Form1);
   // Application.CreateForm(TFormMain, FormMain);

   Application.Run;

end.
