unit ListFilesUtilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function AddPathSep(aPath: string): string;

function CompareArray(aArray1, aArray2: TStringArray): integer;

function IsEmpty(const aString: string): boolean; inline;
function IsNotEmpty(const aString: string): boolean; inline;
procedure SetEmpty(var aString: string); inline;

function IsAssigned(aObject: TObject): boolean; inline;
function IsNotAssigned(aObject: TObject): boolean; inline;

procedure WriteLb;

function SplitInArray(const aString, aSep: string): TStringArray;

implementation

function AddPathSep(aPath: string): string;
begin
  if (aPath[Length(aPath)] <> PathDelim) then
    Result:= aPath + PathDelim
  else
    Result:= aPath;
end;

function CompareArray(aArray1, aArray2: TStringArray): integer;
var
  _iArray: integer;
begin
  Result:= 0;
  if Length(aArray1) <> Length(aArray2) then
    Result:= 9
  else
    for _iArray:= 0 to Length(aArray1) -1 do
      if aArray1[_iArray] <> aArray2[_iArray] then
      begin
        Result:= -9;
        break;
      end;
end;

function IsEmpty(const aString: string): boolean;
begin
  Result:= True;
  if aString <> '' then
    Result:= False;
end;

function IsNotEmpty(const aString: string): boolean;
begin
  Result:= True;
  if aString = '' then
    Result:= False;
end;

procedure SetEmpty(var aString: string);
begin
  aString:= '';
end;

function IsAssigned(aObject: TObject): boolean;
begin
  Result:= false;
  if Assigned(aObject) then
    Result:= True;
end;

function IsNotAssigned(aObject: TObject): boolean;
begin
  Result:= True;
  if Assigned(aObject) then
    Result:= False;
end;

{
  Writing a line break
                                                                              }
procedure WriteLb;
begin
  {$ifdef WINDOWS}
  WriteLn(#13#10);
  {$endif}
  {$ifdef LINUX}
  WriteLn(#10);
  {$endif}
end;

{
  Split string in an array
                                                                              }
function SplitInArray(const aString, aSep: string): TStringArray;
var
  _iStrings, _pSep: integer;
  _Buffer, _Temp: string;
begin
  SetLength(Result, 0);
  if (not IsEmpty(aString)) and (not IsEmpty(aSep)) then
  begin
    _iStrings:= 1;
    _Buffer:= aString;
    _pSep:= Pos(aSep, _buffer);
    repeat { IsNotEmpty(_Buffer) }
    begin
      if _pSep > 0 then
        repeat { _pSep > 0 }
          begin
            SetLength(Result, _iStrings);
            SetEmpty(_Temp);
            if _pSep > 1 then
            begin
              _Temp:= Copy(_Buffer, 0, _pSep - 1);
              _Buffer:= Copy(_Buffer, _pSep + 1, Length(_Buffer) - _pSep);
            end
            else
            begin
              if _pSep = 1 then
                _Buffer:= Copy(_Buffer, 2, Length(_Buffer) - 1);
            end;
            Result[_iStrings - 1]:= _Temp;
            _pSep:= Pos(aSep, _Buffer);
            Inc(_iStrings);
          end;
        until
          _pSep = 0;
      if (_pSep = 0) and IsNotEmpty(_Buffer) then
      begin
        SetLength(Result, _iStrings);
        Result[_iStrings - 1]:= _Buffer;
        SetEmpty(_Buffer);
      end;
    end;
    until
      IsEmpty(_Buffer);
  end;
end;

end.

