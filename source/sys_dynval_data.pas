unit sys_dynval_data;

{$mode objfpc}{$H+}
// I'm using com interfaces here, so that I get reference counting, which I want.
{$Interfaces COM}

interface


uses
  Classes, SysUtils, sys_dynval;

const
  DataStoreObjectGUID = '{385ACAFD-8966-47A0-B215-26375EF695D9}';

type

  IDataStoreObject = interface(IDynamicObject)
    [DataStoreObjectGUID]
    procedure Serialize(aWriter: TDynamicValueWriter); overload;
    procedure Serialize(aStream: TStream); overload;
    function Serialize: UTF8String; overload;
    procedure Deserialize(aReader: TDynamicValueReader); overload;
    procedure Deserialize(aStream: TStream); overload;
    procedure Deserialize(aString: UTF8String); overload;
  end;


implementation

uses
  sys_dynval_json;



end.

