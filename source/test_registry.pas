unit test_registry;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, contnrs;

type
  // Subclassed so that IDE can be configured to ignore expected failures.
  ETestFailure = class(Exception);

  TCallback = procedure of object;
  TTest = procedure(aCallback: TCallback) of object;

  TTestList = specialize TFPGList<TTest>;

  TTestRegistry = class;

  { TTester }

  {$M+}
  TTester = class
  public
    class procedure Register(aRegistry: TTestRegistry);
  end;
  {$M-}

  { TTestRegistry }

  TTestRegistry = class
  private
    fTesters: TObjectList;
    fTestNames: TStringList;
    fTests: TTestList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterTest(const aName: String; aTest: TTest);
    procedure RegisterTests(aClass: TClass);
    function Get(aIndex: Integer): TTest;
    function GetName(aIndex: Integer): String;
  end;

implementation

{ TTester }

class procedure TTester.Register(aRegistry: TTestRegistry);
begin
  aRegistry.RegisterTests(ClassType);
end;

{ TTestRegistry }

constructor TTestRegistry.Create;
begin
  inherited Create;
  fTesters := TObjectList.create(true);
  fTests := TTestList.Create;
  fTestNames := TStringList.Create;
end;

destructor TTestRegistry.Destroy;
begin
  FreeAndNil(fTestNames);
  FreeAndNil(fTests);
  FreeAndNil(fTesters);
  inherited Destroy;
end;

procedure TTestRegistry.RegisterTest(const aName: String; aTest: TTest);
begin
  fTestNames.Add(aName);
  fTests.Add(aTest);

end;

// Code copied from https://github.com/graemeg/fptest/blob/master/src/fpchelper.pas
// which is copied from objpas.inc:  class function TObject.MethodAddress()
procedure TTestRegistry.RegisterTests(aClass: TClass);
type
  TMethodNameRec = packed record
    name: pshortstring;
    addr: pointer;
  end;

  TMethodNameTable = packed record
    count: dword;
    entries: packed array[0..0] of TMethodNameRec;
  end;

  PMethodNameTable =  ^TMethodNameTable;

var
  methodTable: PMethodNameTable;
  i: dword;
  j: Integer;
  vmt: TClass;
  list: TStringList;
  lMethod: TMethod;
  lObject: TObject;
begin
  lObject := nil;
  list := TStringList.Create;
  try
    vmt := aClass;
    while Assigned(vmt) do
    begin
      methodTable := pMethodNameTable((Pointer(vmt) + vmtMethodTable)^);
      if Assigned(MethodTable) then
      begin
        for i := 0 to MethodTable^.count - 1 do
          list.Add(MethodTable^.entries[i].name^);
      end;
      vmt := pClass(pointer(vmt) + vmtParent)^;
    end;
    for j := 0 to list.Count - 1 do
    begin
      if Pos('test',LowerCase(list[j])) = 1 then
      begin
        if lObject = nil then
        begin
          lObject := aClass.Create;
          fTesters.Add(lObject);
        end;
        lMethod.Data := lObject;
        lMethod.Code := lObject.MethodAddress(list[j]);
        // TODO: Note that I can find no way of verifying it's the
        // right method. This doesn't raise an error, as I would expect it
        // to, if the method is invalid. This can cause problems. Perhaps
        // there's some other way to report it as async.
        RegisterTest(aClass.ClassName + '.' + list[j],TTest(lMethod));
      end;
    end;
  finally
    list.Free;
  end;

end;

function TTestRegistry.Get(aIndex: Integer): TTest;
begin
  if aIndex < fTests.Count then
    result := fTests[aIndex]
  else
    result := nil;
end;

function TTestRegistry.GetName(aIndex: Integer): String;
begin
  if aIndex < fTestNames.Count then
    result := fTestNames[aIndex]
  else
    result := '';

end;

end.

