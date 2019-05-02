unit SoundManager;

interface

uses
   System.SysUtils, fmx.media, System.classes, System.types, System.ioutils,
   MMSystem;

type

   TAudioManager = class(TObject)

   var
      SoundState: Boolean;

   const
      StartSoundState = true;
      TmpFileName = 'temp.3gp';
      ResourceBackground = 'BackgroundSound';
      WinSound = 'WinSound';
      CardSound = 'CardSound';
   public
      ChangeSoundImage: procedure(NewState: Boolean) of object;
      class function GetInstance(AOwner: TComponent): TAudioManager;
      procedure ChangeSoundState();
      destructor Destroy; override;
      procedure PlayAudio();
      procedure StopAudio();
      procedure InitAudioPlayer(var AudioPlayer: TMediaPlayer;
        ResourceId: String);

      procedure PlayWinSound();
      procedure StopWinSound();
      procedure PlayCardTurnSound();
   private
      MPCardTurn: TMediaPlayer;
      MPWin: TMediaPlayer;
      AOwner: TComponent;
      class var Instance: TAudioManager;
      constructor Create(AOwner: TComponent);
      procedure PlayWAVfromRES(Name: PWideChar; Options: Cardinal);
   end;

implementation

procedure TAudioManager.PlayWAVfromRES(Name: PWideChar; Options: Cardinal);
var
   hResource: THandle;
   pData: Pointer;
begin
   if (SoundState) then
   begin
      hResource := LoadResource(hInstance, FindResource(hInstance, name,
        RT_RCDATA));
      pData := LockResource(hResource);
      SndPlaySound(pData, SND_MEMORY or Options);
      FreeResource(hResource);
   end;
end;

procedure TAudioManager.ChangeSoundState;
begin
   SoundState := not SoundState;
   ChangeSoundImage(SoundState);
   if (SoundState) then
      PlayAudio
   else
      StopAudio;
end;

constructor TAudioManager.Create;
begin
   SoundState := StartSoundState;
   Self.AOwner := AOwner;
   PlayAudio;
end;

destructor TAudioManager.Destroy;
begin
   inherited;
   if (Instance <> nil) then
      Instance.Free;

end;

class function TAudioManager.GetInstance(AOwner: TComponent): TAudioManager;
begin
   if (Instance = nil) then
      Instance := TAudioManager.Create(AOwner);
   GetInstance := Instance;
end;

procedure TAudioManager.InitAudioPlayer(var AudioPlayer: TMediaPlayer;
  ResourceId: string);
var
   ResStream: TResourceStream;
   TmpFile: string;
begin
   AudioPlayer := TMediaPlayer.Create(AOwner);
   ResStream := TResourceStream.Create(hInstance, ResourceId, RT_RCDATA);
   try

      TmpFile := TPath.Combine(TPath.GetDocumentsPath, ResourceId);
      ResStream.Position := 0;
      ResStream.SaveToFile(TmpFile);
      AudioPlayer.FileName := TmpFile;

   finally
      ResStream.Free;
   end;
end;

procedure TAudioManager.PlayAudio;
begin

   PlayWAVfromRES(ResourceBackground, SND_ASYNC or SND_LOOP);
end;

procedure TAudioManager.PlayCardTurnSound;
begin
   if (MPCardTurn = nil) then
      InitAudioPlayer(MPCardTurn, CardSound);
   MPCardTurn.CurrentTime := 0;
   MPCardTurn.Play;
end;

procedure TAudioManager.PlayWinSound;
begin
   if (MPWin = nil) then
      InitAudioPlayer(MPWin, WinSound);
   MPWin.CurrentTime := 0;
   MPWin.Play;
end;

procedure TAudioManager.StopAudio;
begin
   PlaySound(0, 0, SND_PURGE);
end;

procedure TAudioManager.StopWinSound;
begin
   if (MPWin <> nil) then
   begin
      MPWin.Stop;

   end;
end;

end.
