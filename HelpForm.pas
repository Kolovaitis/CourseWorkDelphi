unit HelpForm;

interface

uses
   System.SysUtils, System.Types, System.UITypes, System.Classes,
   System.Variants,
   FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
   FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
   TDescriptionSet = Record
      ImageBitmap: TBitmap;
      Text: ShortString;
      Title: ShortString;
   End;

   TFormHelp = class(TForm)
      ImRules: TImage;
      Label1: TLabel;
      ImSave: TImage;
      Label2: TLabel;
      ImModes: TImage;
      Label3: TLabel;
      ImAutor: TImage;
      Label4: TLabel;
      ImBackgroundComics: TImage;
      ImBackgroundRed: TImage;
      LTitle: TLabel;
      ImDescription: TImage;
      LDescription: TLabel;
      procedure FormCreate(Sender: TObject);
      procedure OnMenuClick(Sender: TObject);

   public

      MenuPuncts: array [0 .. 3] of TDescriptionSet;

   const
      RulesTitle = '�������';
      SaveTitle = '����������';
      ModesTitle = '������';
      AboutWorkTitle = '� ������';
      RulesText =
        '�� ���� �������� ����� ������������� ������ ���������� ����. ��� ���� ������ ����� ����� ������������� ���� ����� ��������� ����.'
        + ' ����� �������� ��� ����� � �������������� ��. ���� ����� �������, �� ��� �������� ������ ���������, ����� ��� ����� �����������. ���� ���� � ������� ��� �����.';
      SaveText =
        '� ���� ����������� ������� ���������� ���������� ��� ������ ��������� ����. '
        + '��� ��������� ����������� ����������� ���������� ������� ����� ''����������'' � ���� �� ��������� ��������. '
        + '�� ������� �� ��������� ������� ������� ���������� ���������� �� ����� �������.';
      ModesText =
        '� ���� ����������� ��������� � ��������������������� ������ ����. ��������� ����� ������������ ��� ���������� � ���������� ����������. '
        + '��������������������� ����� ��������� ������������� � ��������. ����� ����������� �������� ����� ��� ��������� � ���������� ������.';
      AboutWorkText =
        '������ ��������� �������� �������� ������� �������� ������ 851001 ����������� ������ �� ������ �����.';
      RULES_IMAGE = 'BitmapRules';
      MODES_IMAGE = 'BitmapModes';
      SAVE_IMAGE = 'BitmapSave';
      ABOUT_WORK_IMAGE = 'BitmapAutor';
      procedure ChangePunct(NewIndex: Integer);
      procedure InitPunct(IndexInArray: Integer;
        const BitmapRes, InitText, InitTitle: String);
   end;

var
   FormHelp: TFormHelp;

implementation

{$R *.fmx}

procedure TFormHelp.ChangePunct(NewIndex: Integer);
begin
   With MenuPuncts[NewIndex] do
   begin
      LTitle.Text := Title;
      ImDescription.Bitmap := ImageBitmap;
      LDescription.Text := Text;
   end;
end;

procedure TFormHelp.FormCreate(Sender: TObject);
var
   InStream: TResourceStream;
begin
   InitPunct(0, RULES_IMAGE, RulesText, RulesTitle);
   InitPunct(1, SAVE_IMAGE, SaveText, SaveTitle);
   InitPunct(2, MODES_IMAGE, ModesText, ModesTitle);
   InitPunct(3, ABOUT_WORK_IMAGE, AboutWorkText, AboutWorkTitle);
end;

procedure TFormHelp.InitPunct(IndexInArray: Integer;
  const BitmapRes, InitText, InitTitle: String);
var
   InStream: TResourceStream;
begin
   with MenuPuncts[IndexInArray] do
   begin
      InStream := TResourceStream.Create(HInstance, BitmapRes,
        PChar(RT_RCDATA));
      try
         ImageBitmap := TBitmap.Create;
         ImageBitmap.LoadFromStream(InStream);
      finally
         InStream.Free;
      end;
      Text := InitText;
      Title := InitTitle
   end;
end;

procedure TFormHelp.OnMenuClick(Sender: TObject);
begin
   ChangePunct((Sender as TComponent).Tag);
end;

end.
