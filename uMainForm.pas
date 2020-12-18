unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Ani, FMX.Edit, FMX.Filter.Effects, FMX.Layouts, FMX.Effects, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FMX.ListView, FMX.TabControl, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.DBScope, FMX.ScrollBox, FMX.Memo, FireDAC.Stan.StorageBin, FMX.ComboEdit,
  FMX.EditBox, FMX.ComboTrackBar, System.Threading
  {$IFDEF MSWINDOWS}
  , WinApi.Windows, FMX.MultiView, FMX.Memo.Types
  {$ENDIF}
  ;

type
  TMainForm = class(TForm)
    Rectangle1: TRectangle;
    MaterialOxfordBlueSB: TStyleBook;
    Layout1: TLayout;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ListView1: TListView;
    ProjectsFDMemTable: TFDMemTable;
    BuildButton: TButton;
    Layout3: TLayout;
    PathEdit: TEdit;
    SearchEditButton1: TSearchEditButton;
    ScanButton: TButton;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    ErrorLogMemo: TMemo;
    VertScrollBox1: TVertScrollBox;
    Layout2: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
    Layout6: TLayout;
    ExecParamsEdit: TEdit;
    RSVarsFDMemTable: TFDMemTable;
    RSVArsComboEdit: TComboEdit;
    BindSourceDB2: TBindSourceDB;
    PlatformFDMemTable: TFDMemTable;
    PlatformComboEdit: TComboEdit;
    BindSourceDB3: TBindSourceDB;
    CPUTB: TComboTrackBar;
    LinkFillControlToField: TLinkFillControlToField;
    LinkFillControlToField2: TLinkFillControlToField;
    LinkListControlToField1: TLinkListControlToField;
    StatusLabel: TLabel;
    Layout9: TLayout;
    Layout10: TLayout;
    CleanSwitch: TSwitch;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    TabItem3: TTabItem;
    StatusBar1: TStatusBar;
    procedure ScanButtonClick(Sender: TObject);
    procedure SearchEditButton1Click(Sender: TObject);
    procedure BuildButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FCancel: Boolean;
    function ProcessTask(const AId: Integer): ITask;
    procedure BuildProject(const AId: Integer; const APath: String);
    procedure BuildEnd(const ATime: String);
  {$IFDEF MSWINDOWS}
    function ExeAndWait(ExeNameAndParams: string; ncmdShow: Integer = SW_SHOWNORMAL): Integer;
  {$ENDIF}
  public
    { Public declarations }
  end;
  const
    STS_READY = 'Ready';
    STS_BUILDING = 'Building...';
    STS_SUCCESS = 'Complete';
    STS_FAIL = 'Failed';

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.Diagnostics, System.IOUtils;

procedure TMainForm.ScanButtonClick(Sender: TObject);
var
  LList: TStringDynArray;
  LSearchOption: TSearchOption;
  LItem: String;
begin
  LSearchOption := TSearchOption.soAllDirectories;
  LList := TDirectory.GetFiles(PathEdit.Text, '*.dproj', LSearchOption);
  LList := LList + TDirectory.GetFiles(PathEdit.Text, '*.cbproj', LSearchOption);
  ProjectsFDMemTable.EmptyDataSet;
  ProjectsFDMemTable.BeginBatch;
  for LItem in LList do
    begin
      ProjectsFDMemTable.Append;
      ProjectsFDMemTable.Edit;
      ProjectsFDMemTable.FieldByName('Filename').AsString := ExtractFileName(LItem);
      ProjectsFDMemTable.FieldByName('FullPath').AsString := LItem;
      ProjectsFDMemTable.FieldByName('Status').AsString := STS_READY;
      ProjectsFDMemTable.Post;
    end;
  ProjectsFDMemTable.EndBatch;
  StatusLabel.Text := ProjectsFDMemTable.RecordCount.ToString + ' projects found.';
end;

procedure TMainForm.SearchEditButton1Click(Sender: TObject);
var
  LDirectory: String;
begin
  if SelectDirectory('Open Projects',ExtractFilePath(ParamStr(0)),LDirectory) then
  begin
    PathEdit.Text := LDirectory;
  end;
end;

function TMainForm.ProcessTask(const AId: Integer): ITask;
begin
  Result := TTask.Create(procedure var LIndex: Integer; LPath: String; begin
    for LIndex := 0 to ProjectsFDMemTable.RecordCount-1 do
    begin
      LPath := '';

      TThread.Synchronize(nil,procedure begin
        if ProjectsFDMemTable.Locate('Status',VarArrayOf([STS_READY]),[])=True then
          begin
            LPath := ProjectsFDMemTable.FieldByName('FullPath').AsString;
            ProjectsFDMemTable.Edit;
            ProjectsFDMemTable.FieldByName('Status').AsString := STS_BUILDING;
            ProjectsFDMemTable.Post;
          end;
      end);

      if LPath='' then Exit;

      BuildProject(AId, LPath);

      if FCancel then
        Break;
    end;
  end);
end;

procedure TMainForm.BuildEnd(const ATime: String);
begin
  if FCancel=False then
    StatusLabel.Text := 'Completed in '+ATime+'ms'
  else
    StatusLabel.Text := 'Canceled';

  BuildButton.Tag := 0;
  BuildButton.Text := BuildButton.Hint;

  FCancel := False;
end;

procedure TMainForm.BuildButtonClick(Sender: TObject);
var
  LTasks: array of ITask;
  LThreadCount: Integer;
  LIndex: Integer;
begin
  case BuildButton.Tag of
   0: begin
      FCancel := False;

      ErrorLogMemo.Lines.Clear;

      while not ProjectsFDMemTable.EOF do
      begin
        ProjectsFDMemTable.Edit;
        ProjectsFDMemTable.FieldByName('Status').AsString := STS_READY;
        ProjectsFDMemTable.Post;
        ProjectsFDMemTable.Next;
      end;

      BuildButton.Tag := 1;
      BuildButton.Text := 'Cancel';

      StatusLabel.Text := '';

      LThreadCount := Trunc(CPUTB.Value);

      var StopWatch := TStopWatch.Create;

      StopWatch.Start;

      for LIndex := 1 to LThreadCount do
        begin
          LTasks := LTasks + [ProcessTask(LIndex)];
          LTasks[High(LTasks)].Start;
        end;

        TTask.Run(procedure begin
          TTask.WaitForAll(LTasks);
          TThread.Synchronize(nil,procedure begin
           StopWatch.Stop;
           BuildEnd(StopWatch.ElapsedMilliseconds.ToString);
          end);
        end);
   end;
   1: begin
     FCancel := True;
   end;
  end;

end;

procedure TMainForm.BuildProject(const AId: Integer; const APath: String);
var
  LCurrentFile: String;
  LReturnCode: integer;
  SL: TStringList;
  OutBat: TStringList;
  LAdditionalPath: String;
  LPlatform: String;
  LName: String;
begin

  SL := TStringList.Create;
  SL.LoadFromFile(RSVArsComboEdit.Text);

  LPlatform := 'Win32';
  LName := ExtractFileName(APath).Replace(ExtractFileExt(APath),'');

  OutBat := TStringList.Create;
  try
        LAdditionalPath := '';
        OutBat.Text := Trim(SL.Text);

        if APath.ToUpper.IndexOf('FLATBOX2D')>0 then
          LAdditionalPath := ';DCC_UnitSearchPath=$(DCC_UnitSearchPath)\FlatBox2d;$(DCC_UnitSearchPath)';

        OutBat.Append(Format(ExecParamsEdit.Text, [APAth, PlatformComboEdit.Text, LAdditionalPath, CPUTB.Text]) + ' > ' + 'list'+AId.ToString + '.log');
        if CleanSwitch.IsChecked then OutBat.Append(Format('msbuild "%s" /t:Clean /p:Platform=%s ', [APath, PlatformComboEdit.Text]));
        OutBat.SaveToFile(ExtractFilePath(ParamStr(0)) + 'list'+AId.ToString + '.bat');
        LCurrentFile := 'cmd /c call '+ExtractFilePath(ParamStr(0))+'list'+AId.ToString+'.bat';
        {$IFDEF MSWINDOWS}
        LReturnCode := ExeAndWait(LCurrentFile, SW_HIDE);
        {$ENDIF}
        OutBat.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'list'+AId.ToString + '.log');
        if OutBat.Text.IndexOf('Build succeeded.')>0 then
          begin
            TThread.Synchronize(nil,procedure begin
              if ProjectsFDMemTable.Locate('FullPath',VarArrayOf([APath]),[]) then
              begin
                ProjectsFDMemTable.Edit;
                ProjectsFDMemTable.FieldByName('Status').AsString := STS_SUCCESS;
                ProjectsFDMemTable.Post;
              end;
            end);
          end
        else
          begin
            TThread.Synchronize(nil,procedure begin
              if ProjectsFDMemTable.Locate('FullPath',VarArrayOf([APath]),[]) then
              begin
                ProjectsFDMemTable.Edit;
                ProjectsFDMemTable.FieldByName('Status').AsString := STS_FAIL;
                ProjectsFDMemTable.Post;
              end;
              ErrorLogMemo.Lines.Append(OutBat.Text);
            end);
          end;
        TThread.Synchronize(nil,procedure begin
          Application.ProcessMessages;
        end);


    finally
      OutBat.Free;
      SL.Free;
    end;

end;

{$IFDEF MSWINDOWS}
function TMainForm.ExeAndWait(ExeNameAndParams: string; ncmdShow: Integer = SW_SHOWNORMAL): Integer;
var
    StartupInfo: TStartupInfo;
    ProcessInformation: TProcessInformation;
    Res: Bool;
    lpExitCode: DWORD;
begin
    with StartupInfo do //you can play with this structure
    begin
        cb := SizeOf(TStartupInfo);
        lpReserved := nil;
        lpDesktop := nil;
        lpTitle := nil;
        dwFlags := STARTF_USESHOWWINDOW;
        wShowWindow := ncmdShow;
        cbReserved2 := 0;
        lpReserved2 := nil;
    end;
    Res := CreateProcess(nil, PChar(ExeNameAndParams), nil, nil, True,
        CREATE_DEFAULT_ERROR_MODE
        or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInformation);
    while True do
    begin
        GetExitCodeProcess(ProcessInformation.hProcess, lpExitCode);
        if lpExitCode <> STILL_ACTIVE then
            Break;
        Application.ProcessMessages;
    end;
    Result := Integer(lpExitCode);
end;
{$ENDIF}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  BuildButton.Hint := BuildButton.Text;
end;

end.
