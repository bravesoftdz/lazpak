unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, StdCtrls, ExtCtrls, Spin, EditBtn, StdActns, pkg_unit;

type

  { TFormMain }

  TFormMain = class(TForm)
    actTest: TAction;
    actSavePkg: TAction;
    actOpenDir: TAction;
    alMain: TActionList;
    edAuthor: TEdit;
    actFileExit: TFileExit;
    fileNameEdReadme: TFileNameEdit;
    gbLeft: TGroupBox;
    ImageList16: TImageList;
    lbReadme: TLabel;
    lbLicense: TLabel;
    lbVersion: TLabel;
    lbAuthor: TLabel;
    lbDescription: TLabel;
    lboxSections: TListBox;
    MainMenu: TMainMenu;
    memoReadme: TMemo;
    MemoLicense: TMemo;
    MemoDescription: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    NotebookSections: TNotebook;
    PageDescription: TPage;
    PageReadme: TPage;
    PageScreenshots: TPage;
    panSection: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    ToolBarMain: TToolBar;
    tbtnOpenDir: TToolButton;
    tbtnSavePkg: TToolButton;
    procedure actOpenDirExecute(Sender: TObject);
    procedure actSavePkgExecute(Sender: TObject);
    procedure actTestExecute(Sender: TObject);
    procedure fileNameEdReadmeEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lboxSectionsSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    LazPkgList: TLazPkgList;
    SelectedDir: string;
    Pkg: TLazPkg;
    procedure ReadPkgDir();
    procedure PkgProgressHandler(Sender: TObject);
    procedure FillReadmeFromFile();
  public
    { public declarations }
    procedure PkgToForm();
    procedure FormToPkg();
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

const
  ciPageDescription = 2;
  ciPageLicense     = 1;
  ciPageScreenshots = 0;

{ TFormMain }

procedure TFormMain.lboxSectionsSelectionChange(Sender: TObject; User: boolean);
begin
  if not User then Exit;
  if lboxSections.Selected[0] then NotebookSections.PageIndex:=ciPageDescription
  else if lboxSections.Selected[1] then NotebookSections.PageIndex:=ciPageLicense
  else if lboxSections.Selected[2] then NotebookSections.PageIndex:=ciPageScreenshots;
end;

procedure TFormMain.PkgToForm();
begin
  if not Assigned(Pkg) then Exit;

  MemoDescription.Text:=Pkg.Description;
  edAuthor.Text:=Pkg.Author;
  MemoLicense.Text:=Pkg.License;
  SpinEdit1.Value:=Pkg.VersionMajor;
  SpinEdit2.Value:=Pkg.VersionMinor;
  SpinEdit3.Value:=Pkg.VersionRevision;
  SpinEdit4.Value:=Pkg.VersionBuild;

  fileNameEdReadme.FileName:=Pkg.ReadmeFileName;
  memoReadme.Text:=Pkg.Readme;

  StatusBar.SimpleText:='Package: '+Pkg.PkgName;
end;

procedure TFormMain.FormToPkg();
begin
  if not Assigned(Pkg) then Exit;

  Pkg.Description:=MemoDescription.Text;
  Pkg.Author:=edAuthor.Text;
  Pkg.License:=MemoLicense.Text;
  Pkg.VersionMajor:=SpinEdit1.Value;
  Pkg.VersionMinor:=SpinEdit2.Value;
  Pkg.VersionRevision:=SpinEdit3.Value;
  Pkg.VersionBuild:=SpinEdit4.Value;

  Pkg.ReadmeFileName:=fileNameEdReadme.FileName;
  Pkg.Readme:=memoReadme.Text;

end;

// AMask = '|.jpg|.jpeg|.bmp|'
function FindFiles(APath, AMask: string): string;
var
  n: Integer;
  sPath, sFileExt, sFileName: string;
  sr: TSearchRec;
  slDirs: TStringList;
  slFiles: TStringList;
begin
  slFiles:=TStringList.Create();
  slDirs:=TStringList.Create();

  slDirs.Append(APath);
  while slDirs.Count > 0 do
  begin
    sPath:=slDirs.Strings[slDirs.Count-1];
    slDirs.Delete(slDirs.Count-1);

    // find files and subdirs
    n:=FindFirstUTF8(IncludeTrailingPathDelimiter(sPath)+'*', faAnyFile, sr);
    while n=0 do
    begin
      sFileName:=sr.Name;
      if ((sr.Attr AND faDirectory) <> 0) and (sFileName<>'.') and (sFileName<>'..') then
      begin
        // add folder to list
        slDirs.Append(IncludeTrailingPathDelimiter(sPath)+sFileName);
      end
      else
      begin
        sFileExt:=LowerCase(ExtractFileExt(sFileName));
        if Pos('|'+sFileExt+'|', AMask)>0 then
        begin
          // add file to list
          slFiles.Append(IncludeTrailingPathDelimiter(sPath)+sFileName);
        end;
      end;

      n:=FindNextUTF8(sr);
    end;
  end;
  FreeAndNil(slDirs);

  Result:=slFiles.Text;
  FreeAndNil(slFiles);
end;

procedure TFormMain.ReadPkgDir();
var
  //i: Integer;
  sl: TStringList;
  sLpkName: string;
begin
  // recursively find *.lpk
  sl:=TStringList.Create();
  sl.Text:=FindFiles(SelectedDir, '|.lpk|');

  // parse lpk
  if sl.Count > 0 then
  begin
    sLpkName:=sl.Strings[0];
    if Assigned(Pkg) then FreeAndNil(Pkg);
    Pkg:=TLazPkg.Create();
    Pkg.BasePath:=SelectedDir;
    Pkg.LpkFileName:=sLpkName;
    Pkg.ReadFromLpk();
    PkgToForm();
  end;
  FreeAndNil(sl);
end;

procedure TFormMain.PkgProgressHandler(Sender: TObject);
begin
  if (Sender is TLazPkg) then
  begin
    StatusBar.SimpleText:='Pkg progress: '+IntToStr( (Sender as TLazPkg).Progress );
    Application.ProcessMessages();
  end;
end;

procedure TFormMain.FillReadmeFromFile();
var
  sFileName: string;
begin
  sFileName:=Trim(fileNameEdReadme.FileName);
  if FileExistsUTF8(sFileName) then
  begin
    try
      memoReadme.Lines.LoadFromFile(sFileName);
    finally
    end;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  LazPkgList:=TLazPkgList.Create();
  lboxSections.Selected[0]:=True;
  NotebookSections.PageIndex:=ciPageDescription;
  StatusBar.SimpleText:='Please, open package directory';
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(LazPkgList);
end;

procedure TFormMain.actOpenDirExecute(Sender: TObject);
var
  DirDlg: TSelectDirectoryDialog;
begin
  DirDlg:=TSelectDirectoryDialog.Create(self);
  //DirDlg.HistoryList;
  try
    if DirDlg.Execute then
    begin
      SelectedDir:=DirDlg.FileName;
      ReadPkgDir();
    end;

  finally
    FreeAndNil(DirDlg);
  end;
end;

procedure TFormMain.actSavePkgExecute(Sender: TObject);
begin
  if not Assigned(Pkg) then Exit;
  FormToPkg();
  Pkg.OnProgress:=@PkgProgressHandler;
  Pkg.SavePkg(Pkg.PkgName+'.zip');
end;

procedure TFormMain.actTestExecute(Sender: TObject);
var
  TmpPkg: TLazPkg;
begin

  TmpPkg:=TLazPkg.Create();
  TmpPkg.BasePath:='D:\Work\sources_laz\acs_3\';
  TmpPkg.LpkFileName:='D:\Work\sources_laz\acs_3\packages\laz_acs.lpk';
  TmpPkg.ReadFromLpk();
  LazPkgList.AddPkg(TmpPkg);

  TmpPkg:=TLazPkg.Create();
  TmpPkg.BasePath:='D:\Work\sources_laz\CarddexLib\';
  TmpPkg.LpkFileName:='D:\Work\sources_laz\CarddexLib\carddexlib.lpk';
  TmpPkg.ReadFromLpk();
  LazPkgList.AddPkg(TmpPkg);

  TmpPkg:=TLazPkg.Create();
  TmpPkg.BasePath:='D:\Work\sources_laz\DataPort\';
  TmpPkg.LpkFileName:='D:\Work\sources_laz\DataPort\dataportlasarus.lpk';
  TmpPkg.ReadFromLpk();
  LazPkgList.AddPkg(TmpPkg);

  LazPkgList.SavePkgList('laz_pkg_list.xml');
end;

procedure TFormMain.fileNameEdReadmeEditingDone(Sender: TObject);
begin
  FillReadmeFromFile();
end;

end.

