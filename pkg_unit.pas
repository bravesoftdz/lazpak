unit pkg_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FileUtil,
  Laz2_DOM, laz2_XMLRead, laz2_XMLWrite,
  zipper, strutils, fphttpclient;

type

  { TPkgScreenshot }

  TPkgScreenshot = class(TObject)
  public
    FileName: string;
    Description: string;
    Image: TPicture;
    Thumbnail: TBitmap;
    procedure MakeThumbnail();
  end;

  { TLazPkg }

  TLazPkg = class(TObject)
  private
    FScreenshots: TStringList;
    FFileNameList: TStringList;
    FOnProgress: TNotifyEvent;
    function FGetScreenshotCount(): Integer;
    procedure ZipperProgressHandler(Sender: TObject; const Pct: Double);
  public
    IsGroup: Boolean;
    GroupName: string;
    Parent: TLazPkg;
    PkgName: string;
    Description: string;

    BasePath: string;
    LpkFileName: string;
    Author: string;
    License: string;
    VersionMajor: Integer;
    VersionMinor: Integer;
    VersionRevision: Integer;
    VersionBuild: Integer;
    ReadmeFileName: string;
    Readme: string;
    SiteURL: string;
    Progress: Integer; // 0..100
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    property ScreenshotCount: Integer read FGetScreenshotCount;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    procedure AddScreenshotFromFile(AFileName: string);
    procedure AddScreenshotFromClipboard();
    procedure ReadFromLpk();
    procedure SavePkgMeta();
    procedure SavePkg(AFileName: string);
    function AddMetaToXml(XmlDoc: TXMLDocument; ANode: TDOMNode; AFullMeta: Boolean = False): Boolean;
    function VersionStr(): string;
  end;

  { TLazPkgList }

  TLazPkgList = class(TObject)
  private
    FPkgList: TStringList;
  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    procedure GetHttpMirrors();
    procedure GetHttpPkgList();
    procedure AddPkg(APkg: TLazPkg);
    function SavePkgList(AFileName: string): Boolean;
  end;

implementation

{ TLazPkgList }

procedure TLazPkgList.AfterConstruction();
begin
  inherited AfterConstruction();
  FPkgList:=TStringList.Create();
end;

procedure TLazPkgList.BeforeDestruction();
begin
  FreeAndNil(FPkgList);
  inherited BeforeDestruction();
end;

procedure TLazPkgList.GetHttpMirrors();
begin

end;

procedure TLazPkgList.GetHttpPkgList();
begin

end;

procedure TLazPkgList.AddPkg(APkg: TLazPkg);
begin
  if not Assigned(APkg) then Exit;
  FPkgList.AddObject(APkg.PkgName, APkg);
end;

function TLazPkgList.SavePkgList(AFileName: string): Boolean;
var
  XmlDoc: TXMLDocument;
  RootNode: TDOMNode;
  i: Integer;
  TmpPkg: TLazPkg;
begin
  Result:=False;
  XmlDoc:=TXMLDocument.Create();
  try
    RootNode:=XmlDoc.CreateElement('LazPackages');
    TDOMElement(RootNode).SetAttribute('Count', IntToStr(FPkgList.Count));
    XmlDoc.AppendChild(RootNode);

    for i:=0 to FPkgList.Count-1 do
    begin
      TmpPkg:=(FPkgList.Objects[i] as TLazPkg);
      TmpPkg.AddMetaToXml(XmlDoc, RootNode);
    end;

    WriteXMLFile(XmlDoc, AFileName);
    Result:=True;
  finally
    FreeAndNil(XmlDoc);
  end;
end;

{ TPkgScreenshot }

procedure TPkgScreenshot.MakeThumbnail();
begin

end;

{ TLazPkg }

function TLazPkg.FGetScreenshotCount(): Integer;
begin
  Result:=FScreenshots.Count;
end;

procedure TLazPkg.ZipperProgressHandler(Sender: TObject; const Pct: Double);
begin
  Progress:=Round(Pct);
  //if (Sender is TZipper) then Progress:=(Sender as TZipper).;
  if Assigned(OnProgress) then OnProgress(Self);
end;

procedure TLazPkg.AfterConstruction();
begin
  inherited AfterConstruction();
  FFileNameList:=TStringList.Create();
  FScreenshots:=TStringList.Create();
end;

procedure TLazPkg.BeforeDestruction();
begin
  FreeAndNil(FScreenshots);
  FreeAndNil(FFileNameList);
  inherited BeforeDestruction();
end;

procedure TLazPkg.AddScreenshotFromFile(AFileName: string);
begin

end;

procedure TLazPkg.AddScreenshotFromClipboard();
begin

end;

procedure TLazPkg.ReadFromLpk();
var
  XmlDoc: TXMLDocument;
  NodePkg, Node: TDOMNode;
  fs: TFileStream;

begin
  if not FileExistsUTF8(LpkFileName) then Exit;
  fs:=TFileStream.Create(LpkFileName, fmOpenRead);
  // parse lpk
  try
    ReadXMLFile(XmlDoc, fs);
  finally
    FreeAndNil(fs);
  end;

  if not Assigned(XmlDoc) then Exit;

  //NodeConfig:=XmlDoc.DocumentElement.FindNode('CONFIG');
  NodePkg:=XmlDoc.DocumentElement.FindNode('Package');
  //if Assigned(NodeConfig) then
  begin
    //NodePkg:=NodeConfig.FindNode('Package');
    if Assigned(NodePkg) then
    begin
      Node:=NodePkg.FindNode('Name');
      if Assigned(Node) then PkgName:=TDOMElement(Node).GetAttribute('Value');

      Node:=NodePkg.FindNode('Author');
      if Assigned(Node) then Author:=TDOMElement(Node).GetAttribute('Value');

      Node:=NodePkg.FindNode('Description');
      if Assigned(Node) then Description:=TDOMElement(Node).GetAttribute('Value');

      Node:=NodePkg.FindNode('License');
      if Assigned(Node) then License:=TDOMElement(Node).GetAttribute('Value');

      Node:=NodePkg.FindNode('Version');
      if Assigned(Node) then
      begin
        VersionMajor:=StrToIntDef(TDOMElement(Node).GetAttribute('Major'), 0);
        VersionMinor:=StrToIntDef(TDOMElement(Node).GetAttribute('Minor'), 0);
        VersionRevision:=StrToIntDef(TDOMElement(Node).GetAttribute('Release'), 0);
        VersionBuild:=StrToIntDef(TDOMElement(Node).GetAttribute('Build'), 0);
      end;
    end;
  end;
end;

procedure TLazPkg.SavePkgMeta();
var
  XmlDoc: TXMLDocument;
  RootNode: TDOMNode;
begin
  XmlDoc:=TXMLDocument.Create();
  try
    RootNode:=XmlDoc.CreateElement('LazPackage');
    TDOMElement(RootNode).SetAttribute('Name', Self.PkgName);
    XmlDoc.AppendChild(RootNode);

    Self.AddMetaToXml(XmlDoc, RootNode, True);

    WriteXMLFile(XmlDoc, IncludeTrailingPathDelimiter(BasePath)+'lazpkg.xml');
  finally
    FreeAndNil(XmlDoc);
  end;
end;

procedure TLazPkg.SavePkg(AFileName: string);
var
  AZipper: TZipper;
  szPathEntry: string;
  i: Integer;
  ZEntries: TZipFileEntries;
  TheFileList: TStringList;
  RelativeDirectory: string;

begin
  SavePkgMeta();
  // zip files
  AZipper := TZipper.Create();
  try
    RelativeDirectory:=BasePath;
    AZipper.Filename:=AFileName;
    AZipper.FileComment:=Description;
    AZipper.OnProgress:=@ZipperProgressHandler;
    AZipper.Clear();
    ZEntries := TZipFileEntries.Create(TZipFileEntry);
    // Verify valid directory
    if DirPathExists(RelativeDirectory) then
    begin
      // Construct the path to the directory BELOW RelativeDirectory
      // If user specifies 'C:\MyFolder\Subfolder' it returns 'C:\MyFolder\'
      // If user specifies 'C:\MyFolder' it returns 'C:\'
      // If user specifies 'C:\' it returns 'C:\'
      i:=RPos(PathDelim, ChompPathDelim(RelativeDirectory));
      szPathEntry:=LeftStr(RelativeDirectory, i);

      // Use the FileUtils.FindAllFiles function to get everything (files and folders) recursively
      //TheFileList:=TStringList.Create();
      try
        TheFileList:=FindAllFiles(RelativeDirectory);
      except
        TheFileList:=nil;
      end;

      if Assigned(TheFileList) then
      begin
        for i:=0 to TheFileList.Count -1 do
        begin
          // Make sure the RelativeDirectory files are not in the root of the ZipFile
          ZEntries.AddFileEntry(TheFileList[i], CreateRelativePath(TheFileList[i], szPathEntry));
        end;
        FreeAndNil(TheFileList);
      end;
      // zip files
      try
        if (ZEntries.Count > 0) then AZipper.ZipFiles(ZEntries);
      except
        On E: EZipError do
          E.CreateFmt('Zipfile could not be created%sReason: %s', [LineEnding, E.Message])
      end;
    end;
  finally
    FreeAndNil(ZEntries);
    FreeAndNil(AZipper);
  end;
end;

function TLazPkg.AddMetaToXml(XmlDoc: TXMLDocument; ANode: TDOMNode;
  AFullMeta: Boolean): Boolean;
var
  PkgNode, Node: TDOMNode;

procedure AddNameValue(AName, AValue: string);
begin
  if Trim(AValue)<>'' then
  begin
    Node:=XmlDoc.CreateElement(AName);
    TDOMElement(Node).SetAttribute('Value', AValue);
    PkgNode.AppendChild(Node);
  end;
end;

begin
  Result:=True;
  PkgNode:=XmlDoc.CreateElement('LazPackage');
  TDOMElement(PkgNode).SetAttribute('Name', Self.PkgName);
  TDOMElement(PkgNode).SetAttribute('Version', Self.VersionStr());
  ANode.AppendChild(PkgNode);

  AddNameValue('Description', Self.Description);
  AddNameValue('Author', Self.Author);
  AddNameValue('Group', Self.GroupName);
  AddNameValue('License', Self.License);
  AddNameValue('SiteURL', Self.SiteURL);
  AddNameValue('Readme', Self.Readme);

  if not AFullMeta then Exit;

  // Version
  Node:=XmlDoc.CreateElement('Version');
  if Self.VersionMajor <> 0 then
    TDOMElement(Node).SetAttribute('Major', IntToStr(Self.VersionMajor));
  if Self.VersionMinor <> 0 then
    TDOMElement(Node).SetAttribute('Minor', IntToStr(Self.VersionMinor));
  if Self.VersionRevision <> 0 then
    TDOMElement(Node).SetAttribute('Release', IntToStr(Self.VersionRevision));
  if Self.VersionBuild <> 0 then
    TDOMElement(Node).SetAttribute('Build', IntToStr(Self.VersionBuild));
  PkgNode.AppendChild(Node);

  AddNameValue('ReadmeFileName', Self.ReadmeFileName);
  AddNameValue('LpkFileName', Self.LpkFileName);

end;

function TLazPkg.VersionStr(): string;
begin
  Result:=IntToStr(VersionMajor)
  +'.'+IntToStr(VersionMinor)
  +'.'+IntToStr(VersionRevision)
  +'.'+IntToStr(VersionBuild);
end;

end.

