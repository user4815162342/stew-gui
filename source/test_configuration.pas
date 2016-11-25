unit test_configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, test_sys_types, test_sys_dynval, test_sys_async,
  test_sys_localfile, test_longstringmap, test_sys_filecache_local,
  test_stew_properties, test_stew_project, test_gui;

const
  TestsToRun: array[0..8] of TTestSpecClass =
    (TTypesSpec,
     TDynValSpec,
     TAsyncSpec,
     TLocalFileSpec,
     TLongStringMapSpec,
     TLocalFileCacheSpec,
     TPropertiesSpec,
     TProjectSpec,
     TGUISpec);


implementation

end.

