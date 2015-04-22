unit stewtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TExceptionEvent = procedure(Sender: TObject; aError: Exception) of object;
  TExceptionMessageEvent = procedure(Sender: TObject; aError: String) of object;


implementation

end.

