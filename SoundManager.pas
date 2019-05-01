unit SoundManager;


interface

uses
   System.SysUtils, fmx.media, System.classes, System.types, system.ioutils;

type

   TAudioManager = class(TObject)
   private
      MPBackground: TMediaPlayer;
      class var Instance: TAudioManager;
      constructor Create();


   var
      SoundState:Boolean;

   const
      StartSoundState=true;
      TmpFileName = 'temp.3gp';
      ResourceBackground = 'background.mp3';
   public
      ChangeSoundImage: procedure (NewState: Boolean) of object;
      class function GetInstance(): TAudioManager;
      procedure ChangeSoundState();
      destructor Destroy; override;
      procedure PlayAudio();
      procedure InitAudioPlayer(ResourceId:String);
   end;



implementation




procedure TAudioManager.ChangeSoundState;
begin
   SoundState:=not SoundState;
ChangeSoundImage(SoundState);
end;

constructor TAudioManager.Create;
begin
  SoundState:=StartSoundState;
  InitAudioPlayer(ResourceBackground);
end;

destructor TAudioManager.Destroy;
begin
   inherited;
   if (Instance <> nil) then
      Instance.Free;

end;


class function TAudioManager.GetInstance: TAudioManager;
begin
   if (Instance = nil) then
      Instance := TAudioManager.Create;
   GetInstance := Instance;
end;


procedure TAudioManager.InitAudioplayer(ResourceID: string);
var
  ResStream: TResourceStream;
  TmpFile: string;
begin

  ResStream := TResourceStream.Create(HInstance, ResourceID, RT_RCDATA);
  try

    TmpFile := TPath.Combine(TPath.GetDocumentsPath, TmpFileName);
    ResStream.Position := 0;
    ResStream.SaveToFile(TmpFile);
    MPBackground.FileName := TmpFile;


  finally
    ResStream.Free;
  end;

end;

procedure TAudioManager.PlayAudio;
begin
if(SoundState)then
MPBackground.Play;
end;

end.
