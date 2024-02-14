{========================================================================================
  Module :
  Description :
  Author : Frédéric Libaud
  **************************************************************************************
  History
  --------------------------------------------------------------------------------------
========================================================================================}
unit ListFiles;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  { TListFiles declaration }

  TFoundFile = class (TCollectionItem)
      private
        // Members
        _File: TFileName;
        // Methods
        function GetName: TFileName;
        function GetPath: TFileName;
        function GetExtension: string;
        function GetNameWithPath: string;
        function GetSize: LongInt;
        function GetAttributs: LongInt;
        function GetExcludeAttributs: LongInt;
        function GetDateTime: TDateTime;
        function GetAge: LongInt;
      public
        constructor Create(aCollection: TCollection; const aFile: TFileName); overload;
        // Properties
        property Name: TFileName read GetName;
        property Path: TFileName read GetPath;
        property Extension: string read GetExtension;
        property NameWithPath: string read GetNamewithPath;
        property Size: LongInt read GetSize;
        property Attributs: LongInt read GetAttributs;
        property ExcludeAttributs: LongInt read GetExcludeAttributs;
        property DateTime: TDateTime read GetDateTime;
        property Age: LongInt read GetAge;
    end;

  TFoundFileClass = class of TFoundFile;

  TFoundFilesCollection = class (TCollection)
      private
        // Methods
        function GetAt(const aIndex: integer): TFoundFile;
        function GetItemClass: TFoundFileClass;
      public

        // Methods
        function Add(const aFileName: TFileName): TFoundFile;
        // Properties
        property At[const aIndex: integer]: TFoundFile read GetAt;
        property ItemClass: TFoundFileClass read GetItemClass;
    end;

  TOutputFormat = (ofCSV, ofList);

  TSearchFiles = class
      private
        // Members
        oFileStream: TFileStream;
        oFiles: TFoundFilesCollection;
        _RootDirectory: TFileName;
        _FilesFilters: TStringArray;
        _Output: string;
        _OutputFormat: TOutputFormat;
        _Working: boolean;
        // Methods
        procedure RecurseSearchFiles(const aDirectory: TFileName);
        procedure SearchFiles;
        procedure InternalRefresh;
        procedure WriteOutputList(aFoundFile: TFoundFile);
        procedure WriteOutputCSV(aFoundFile: TFoundFile);
        // Access Methods
        procedure SetRootDirectory(const aRootDirectory: TFileName);
        procedure SetFilesFilters(const aFilesFilters: TStringArray);
        procedure SetOutput(const aOutput: string);
        function GetCountOfFiles: integer;
        function GetFile(const aIndex: integer): TFoundFile;
        function GetFiltersAsString: string;
      public
        constructor Create;
        destructor Destroy;
        // Methods
        procedure Refresh;
        // Properties
        property RootDirectory: TFileName read _RootDirectory
                                          write SetRootDirectory;
        property FilesFilters: TStringArray read _FilesFilters
                                            write SetFilesFilters;
        property Output: string read _Output
                                write SetOutput;
        property OutputFormat: TOutputFormat read _OutputFormat
                                      write _OutputFormat;
        property CountOfFiles: integer read GetCountOfFiles;
        property Files[const aIndex: integer]: TFoundFile read GetFile;
        property Stream: TFileStream read oFileStream;

    end;

  { End of TListFiles declaration }

implementation

uses
  FileUtil, LazFileUtils, ListFilesUtilities;

{ TFoundFile implementation }

constructor TFoundFile.Create(aCollection: TCollection; const aFile: TFileName);
begin
  inherited Create(aCollection);
  _File:= aFile;
end;

function TFoundFile.GetName: TFileName;
begin
  Result:= ExtractFileName(_File);
end;

function TFoundFile.GetPath: TFileName;
begin
  Result:= ExtractFilePath(_File);
end;

function TFoundFile.GetExtension: string;
begin
  Result:= ExtractFileExt(_File);
end;

function TFoundFile.GetNameWithPath: string;
begin
  Result:= _File;
end;

function TFoundFile.GetSize: LongInt;
{var
  _FileSize: File of byte;}
begin
  {System.Assign(_FileSize, _File);
  try
    Reset(_FileSize);
    Result:= _FileSize;
  finally
    Close(_FileSize);
  end;}
  Result:= FileSize(_File);
end;

function TFoundFile.GetAttributs: LongInt;
begin
  Result:= FileGetAttr(_File);
end;

function TFoundFile.GetExcludeAttributs: LongInt;
begin

end;

function TFoundFile.GetDateTime: TDateTime;
var
  _F: longint;
begin
  _F:= FileOpen(_File, fmOpenRead);
  try
    Result:= FileDateToDateTime(FileGetDate(_F));
  finally
    FileClose(_F);
  end;
end;

function TFoundFile.GetAge: LongInt;
begin
  Result:= FileAge(_File);
end;

{ End of TFoundFile implementation }

{ TFoundFilesCollection implementation }

function TFoundFilesCollection.GetAt(const aIndex: integer): TFoundFile;
begin
  Result:= (Items[aIndex] as TFoundFile);
end;

function TFoundFilesCollection.GetItemClass: TFoundFileClass;
begin
  Result:= TFoundFile;
end;

function TFoundFilesCollection.Add(const aFileName: TFileName): TFoundFile;
begin
  Result:= ItemClass.Create(Self, aFileName);
end;

{ End of TFoundFilesCollection implementation }

{ TSearchFiles implementation }

constructor TSearchFiles.Create;
begin
  inherited Create;
  oFiles:= TFoundFilesCollection.Create(TFoundFile);
end;

destructor TSearchFiles.Destroy;
begin
  oFiles.Destroy;
  inherited Destroy;
end;

// Private methods

procedure TSearchFiles.RecurseSearchFiles(const aDirectory: TFileName);
var
  oFoundFile: TFoundFile;
  _FilesSearchRec: TSearchRec;

  function VerifyExt(const aExt: string): boolean;
  var
    _iFilter: integer;
  begin
    Result:= False;
    if _FilesFilters[0] = '*.*' then
      Result:= True
    else
    for _iFilter:= 0 to Length(_FilesFilters) - 1 do
      if _FilesFilters[_iFilter] = aExt then
      begin
        Result:= True;
        break;
      end;
  end;

begin
  if FindFirst(AddPathSep(aDirectory) + '*', faAnyFile, _FilesSearchRec) = 0 then
  try
    repeat
      if not ((_FilesSearchRec.Attr and faDirectory) <> 0) then
      begin
        {$ifdef LINUX}
        if (_FilesSearchRec.Name <> '.') and (_FilesSearchRec.Name <> '..') then
        {$endif}
          if VerifyExt('*' + ExtractFileExt(_FilesSearchRec.Name)) then
          begin
            oFoundFile:= TFoundFile.Create(oFiles, AddPathSep(aDirectory) + _FilesSearchRec.Name);
            //oFiles.Add(oFoundFile);
            if IsNotEmpty(_Output) then
              case _OutputFormat of
                ofList: WriteOutputList(oFoundFile);
                ofCSV: WriteOutputCSV(oFoundFile);
              end;
            end;
      end
      else
      {if (_FilesSearchRec.Attr and faDirectory) = 0 then}
        {$ifdef LINUX}
        if (_FilesSearchRec.Name <> '.') and (_FilesSearchRec.Name <> '..') then
        {$endif}
          RecurseSearchFiles(AddPathSep(aDirectory) + _FilesSearchRec.Name);
    until
      FindNext(_FilesSearchRec) <> 0;
  finally
    FindClose(_FilesSearchRec);
  end;
end;

procedure TSearchFiles.SearchFiles;
begin
  if IsNotEmpty(_Output) then
    oFileStream:= TFileStream.Create(_Output, fmCreate + fmOpenReadWrite + fmShareExclusive);
  {for _iFilter:= 0 to Length(_FilesFilters) - 1 do}
  RecurseSearchFiles(RootDirectory);
  if IsAssigned(oFileStream) then
  begin
    oFileStream.Flush;
    oFileStream.Destroy;
  end;
end;

procedure TSearchFiles.InternalRefresh;
begin
  if oFiles.Count > 0 then
    oFiles.Clear;
  if Length(_FilesFilters) = 0 then
  begin
    SetLength(_FilesFilters, 1);
    _FilesFilters[0]:= '*.*';
  end;
  SearchFiles;
end;

procedure TSearchFiles.WriteOutputList(aFoundFile: TFoundFile);
var
  _Temp: string;
begin
  _Temp:= aFoundFile.NameWithPath + #10;
  oFileStream.Write(_Temp, Length(_Temp));
end;

procedure TSearchFiles.WriteOutputCSV(aFoundFile: TFoundFile);
begin

end;

// Private Access Methods

procedure TSearchFiles.SetRootDirectory(const aRootDirectory: TFileName);
begin
  if not _Working then
    if aRootDirectory <> _RootDirectory then
      _RootDirectory:= aRootDirectory;
end;

procedure TSearchFiles.SetFilesFilters(const aFilesFilters: TStringArray);

  function CorrectFilesFilters(const aFilesFilters: TStringArray): TStringArray;
  var
    _iFilter: integer;
  begin
    SetLength(Result, Length(aFilesFilters));
    for _iFilter:= 0 to Length(aFilesFilters) - 1 do
      if Copy(aFilesFilters[_iFilter], 1, 2) <> '*.' then
        Result[_iFilter]:= '*.' + aFilesFilters[_iFilter]
      else
        Result[_iFilter]:= aFilesFilters[_iFilter];
  end;

begin
  if not _Working then
    if not (CompareArray(aFilesFilters, _FilesFilters) = 0) then
      _FilesFilters:= CorrectFilesFilters(aFilesFilters);
end;

procedure TSearchFiles.SetOutput(const aOutput: string);
begin
  if not _Working then
    if aOutput <> _Output then
      _Output:= aOutput;
end;

function TSearchFiles.GetCountOfFiles: integer;
begin
  Result:= oFiles.Count;
end;

function TSearchFiles.GetFile(const aIndex: integer): TFoundFile;
begin
  Result:= oFiles.At[aIndex];
end;

function TSearchFiles.GetFiltersAsString: string;
begin

end;

// Public Method's

procedure TSearchFiles.Refresh;
begin
  InternalRefresh;
end;

{ End of TSearchFiles implementation }

end.

