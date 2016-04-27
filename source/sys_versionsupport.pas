Unit sys_versionsupport;
// From http://forum.lazarus.freepascal.org/index.php/topic,13957.msg73542.html#msg73542
// With modifications marked by NMS.

{$mode objfpc}{$H+}

Interface

(*
  Building on the excellent vinfo.pas supplied by Paul Ishenin and available elsewhere on these Lazarus
  Forums
    - I hid the TVersionInfo class from the end user to simplify their (mine) number of required Uses...
    - Added defensive code to TVersionInfo if no build info is compiled into the exe
    - Deduced GetResourceStrings - works under Linux 64/GTK2 with Lazarus 0.9.30, but fails under
      Win XP 32bit/Lazarus 0.9.29 - suspecting my install as the lazresexplorer example also fails
      for me under Lazarus 0.9.29, but works with Lazarus 0.9.30

  Trawled through IDE source code, FPC source code and Lazarus supplied example program lasresexplorer
  to find the other defines and lookups...

  End user only needs to use VersionSupport - no other units necessary for their project.

  Jedi CodeFormatter seems to fail on the {$I %VARIABLE%} references, so sticking them all in here
  means end user code can be neatly formatted using Jedi CodeFormatter

  Other interesting includes I picked up in my travels are...
  //  {$I %HOME%} = User Home Directory
  //  {$I %FILE%} = Current pas file
  //  {$I %LINE%} = current line number

  Mike Thompson - mike.cornflake@gmail.com
  July 24 2011
*)

Uses
  Classes, SysUtils, CustApp;

type
  TAboutText = record
    Title: String;
    Copyright: String;
    Description: String;
    BuildInfo: String;
  end;

function GetAboutText(Application: TCustomApplication): TAboutText;
Function GetFileVersion: String;
Function GetProductVersion: String;
Function GetCompiledDate: String;
Function GetCompilerInfo: String;
Function GetTargetInfo: String;
Function GetOS: String;
Function GetResourceStrings(oStringList : TStringList) : Boolean;
Function GetLCLVersion: String;
function GetWidgetSet: string;
function GetResourceString(name: string): string; // NMS: Added

Const
  WIDGETSET_GTK        = 'GTK widget set';
  WIDGETSET_GTK2       = 'GTK 2 widget set';
  WIDGETSET_WIN        = 'Win32/Win64 widget set';
  WIDGETSET_WINCE      = 'WinCE widget set';
  WIDGETSET_CARBON     = 'Carbon widget set';
  WIDGETSET_QT         = 'QT widget set';
  WIDGETSET_fpGUI      = 'fpGUI widget set';
  WIDGETSET_OTHER      = 'Other gui';

Implementation

Uses
  resource, versiontypes, versionresource, LCLVersion, InterfaceBase, FileUtil;

Type
  TVersionInfo = Class
  strict private
    FBuildInfoAvailable: Boolean;
    FVersResource: TVersionResource;
    Function GetFixedInfo: TVersionFixedInfo;
    Function GetStringFileInfo: TVersionStringFileInfo;
    Function GetVarFileInfo: TVersionVarFileInfo;
  public
    Constructor Create;
    Destructor Destroy; override;

    Procedure Load(Instance: THandle);

    Property BuildInfoAvailable: Boolean Read FBuildInfoAvailable;

    Property FixedInfo: TVersionFixedInfo Read GetFixedInfo;
    Property StringFileInfo: TVersionStringFileInfo Read GetStringFileInfo;
    Property VarFileInfo: TVersionVarFileInfo Read GetVarFileInfo;
  End;

function GetWidgetSet: string;
begin
  case WidgetSet.LCLPlatform of
    lpGtk:   Result := WIDGETSET_GTK;
    lpGtk2:  Result := WIDGETSET_GTK2;
    lpWin32: Result := WIDGETSET_WIN;
    lpWinCE: Result := WIDGETSET_WINCE;
    lpCarbon:Result := WIDGETSET_CARBON;
    lpQT:    Result := WIDGETSET_QT;
    lpfpGUI: Result := WIDGETSET_fpGUI;
  else
    Result:=WIDGETSET_OTHER;
  end;
end;

function GetCompilerInfo: String;
begin
  Result := 'FPC '+{$I %FPCVERSION%};
end;

function GetTargetInfo: String;
begin
  Result := {$I %FPCTARGETCPU%}+' - '+{$I %FPCTARGETOS%};
end;

function GetOS: String;
Begin
  Result := {$I %FPCTARGETOS%};
End;

function GetLCLVersion: String;
begin
  Result := 'LCL '+lcl_version;
end;

function GetCompiledDate: String;
Var
  sDate, sTime: String;
Begin
  sDate := {$I %DATE%};
  sTime := {$I %TIME%};

  Result := sDate + ' at ' + sTime;
End;

{ Routines to expose TVersionInfo data }

Var
  FInfo: TVersionInfo;

Procedure CreateInfo;
Begin
  If Not Assigned(FInfo) Then
  Begin
    FInfo := TVersionInfo.Create;
    FInfo.Load(HINSTANCE);
  End;
End;

function GetResourceStrings(oStringList: TStringList): Boolean;
Var
  i, j : Integer;
  oTable : TVersionStringTable;
begin
  CreateInfo;

  oStringList.Clear;
  Result := False;

  If FInfo.BuildInfoAvailable Then
  Begin
    Result := True;
    For i := 0 To FInfo.StringFileInfo.Count-1 Do
    Begin
      oTable := FInfo.StringFileInfo.Items[i];

      For j := 0 To oTable.Count-1 Do
        If Trim(oTable.ValuesByIndex[j])<>'' Then
          oStringList.Values[oTable.Keys[j]] := oTable.ValuesByIndex[j];
    end;
  end;
end;

function GetResourceString(name: string): string;
Var
  i : Integer;
  oTable : TVersionStringTable;
begin
  CreateInfo;

  Result := '';

  If FInfo.BuildInfoAvailable Then
  Begin
    For i := 0 To FInfo.StringFileInfo.Count-1 Do
    Begin
      oTable := FInfo.StringFileInfo.Items[i];
      try
         Result := Result + oTable.Values[Name];
      except
      on E: EKeyNotFoundException do
      // ignore, just assume a blank value.
      end;
    end;
  end;
end;

Function FileVersionToString(PV: TFileProductVersion): String;
Begin
  Result := Format('%d.%d.%d.%d', [PV[0], PV[1], PV[2], PV[3]]);
End;

// NMS: Separated Product Version, for which I only have a major and minor,
// from file version, for which I have release and patch.
Function ProductVersionToString(PV: TFileProductVersion): String;
Begin
  Result := Format('%d.%d', [PV[0], PV[1]]);
End;

function GetProductVersion: String;
Begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := ProductVersionToString(FInfo.FixedInfo.ProductVersion)
  Else
    Result := 'No build information available';
End;

function GetFileVersion: String;
Begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := FileVersionToString(FInfo.FixedInfo.FileVersion)
  Else
    Result := 'No build information available';
End;

{ TVersionInfo }

Function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
Begin
  Result := FVersResource.FixedInfo;
End;

Function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
Begin
  Result := FVersResource.StringFileInfo;
End;

Function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
Begin
  Result := FVersResource.VarFileInfo;
End;

Constructor TVersionInfo.Create;
Begin
  Inherited Create;

  FVersResource := TVersionResource.Create;
  FBuildInfoAvailable := False;
End;

Destructor TVersionInfo.Destroy;
Begin
  FVersResource.Free;

  Inherited Destroy;
End;

Procedure TVersionInfo.Load(Instance: THandle);
Var
  Stream: TResourceStream;
  ResID: Integer;
  Res: TFPResourceHandle;
Begin
  FBuildInfoAvailable := False;
  ResID := 1;

  // Defensive code to prevent failure if no resource available...
  // TODO: Get a compiler hint here. Worry about it and test it when
  // I try to rebuild this on another platform.
  Res := FindResource(Instance, {%H-}PChar(PtrInt(ResID)), PChar(RT_VERSION));
  If Res = 0 Then
    Exit;

  Stream := TResourceStream.CreateFromID(Instance, ResID, PChar(RT_VERSION));
  Try
    FVersResource.SetCustomRawDataStream(Stream);

    // access some property to load from the stream
    FVersResource.FixedInfo;

    // clear the stream
    FVersResource.SetCustomRawDataStream(nil);

    FBuildInfoAvailable := True;
  Finally
    Stream.Free;
  End;
End;

function GetAboutText(Application: TCustomApplication): TAboutText;
begin
  result.Title := Application.Title + ' v ' + sys_versionsupport.GetProductVersion;
  Result.Copyright := sys_versionsupport.GetResourceString('LegalCopyright');
  result.Description := sys_versionsupport.GetResourceString('FileDescription');
  result.BuildInfo := 'Executable: ' + ExtractFileNameOnly(Application.ExeName) + ' v ' + sys_versionsupport.GetFileVersion + LineEnding +
                            'Compiled On: ' + sys_versionsupport.GetCompiledDate + LineEnding +
                            'Compiler: ' + sys_versionsupport.GetCompilerInfo + LineEnding +
                            'Target Platform: ' + sys_versionsupport.GetTargetInfo + LineEnding +
                            'Lazarus Component Library: ' + sys_versionsupport.GetLCLVersion + LineEnding +
                            'WidgetSet: ' + sys_versionsupport.GetWidgetSet;
end;



Initialization
  FInfo := nil;

Finalization
  If Assigned(FInfo) Then
    FInfo.Free;
End.
