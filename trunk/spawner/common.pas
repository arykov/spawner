(*
  Copyright (C) 2007 by Seth Grover.  All rights reserved.

  This file is part of the Spawner Data Generator.

  The Spawner Data Generator is free software; you can
  redistribute it and/or modify it under the terms of the GNU General
  Public License (GPL) as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  The Spawner Data Generator is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with the Spawner Data Generator; if not, write to
  the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
  02110-1301  USA
*)

unit common;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, names, {$IFDEF WINDOWS}Windows{$ELSE}Libc{$ENDIF};

type


// -Field classes---------------------------------------------------------------
  TField = class
  protected
    FName : string;
    FType : string;
    FSubType : string;
  public
    property Name : string read FName;
    property ItemType : string read FType;
    property ItemSubType : string read FSubType;
    function GetField(const quoteChar : string = '') : string; virtual;
    function GetAsString : string; virtual;
    procedure Reset; virtual;
    constructor Create(const name : string; const theType : string; const theSubType : string);
  end;
  PField = ^TField;
  
  TIntegerRangeField = class(TField)
  private
    FLow : longint;
    FHigh : longint;
  public
    property LowVal : longint read FLow;
    property HighVal : longint read FHigh;
    function GetField(const quoteChar : string = '') : string; override;
    function GetAsString : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string;
                       const low : longint;
                       const high : longint);
  end;
  
  TRealRangeField = class(TField)
  private
    FLow : Double;
    FHigh : Double;
    FDecimalPlaces : byte;
  public
    property LowVal : Double read FLow;
    property HighVal : Double read FHigh;
    property DecimalPlaces : byte read FDecimalPlaces;
    function GetField(const quoteChar : string = '') : string; override;
    function GetAsString : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string;
                       const low : Double;
                       const high : Double;
                       const decimalPlaces : byte);
  end;
  
  TDateTimeRangeField = class(TField)
  private
    FLow : TDateTime;
    FHigh : TDateTime;
    FIncludeDate : boolean;
    FIncludeTime : boolean;
    FCurrentOnly : boolean;
  public
    property LowVal : TDateTime read FLow;
    property HighVal : TDateTime read FHigh;
    property IncludeDate : boolean read FIncludeDate;
    property IncludeTime: boolean read FIncludeTime;
    property CurrentOnly: boolean read FCurrentOnly;
    function GetField(const quoteChar : string = '') : string; override;
    function GetAsString : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string;
                       const low : TDateTime;
                       const high : TDateTime;
                       const includeDate : boolean;
                       const includeTime : boolean;
                       const currentOnly : boolean);
  end;
  
  TSetField = class(TField)
  private
    FSet : TStringList;
  public
    function GetSetString : string;
    function GetField(const quoteChar : string = '') : string; override;
    function GetAsString : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string;
                       const text : string);
    destructor Destroy; override;
  end;
  
  TSequenceField = class(TField)
  private
    FStart : longint;
    FDuplicate : longint;
    FStride : longint;
    FLastNumber : longint;
    FCurrentDuplicate : longint;
  public
    property Start : longint read FStart;
    property Duplicate : longint read FDuplicate;
    property Stride : longint read FStride;
    function GetField(const quoteChar : string = '') : string; override;
    function GetAsString : string; override;
    procedure Reset; override;
    constructor Create(const name : string; const theType : string; const theSubType : string;
                       const start : longint;
                       const duplicate : longint;
                       const stride : longint);
  end;
  
  TNameField = class(TField)
  private
    FIncludeFirst : boolean;
    FIncludeLast : boolean;
    FFemale : boolean;
    FMale : boolean;
  public
    property IncludeFirst : boolean read FIncludeFirst;
    property IncludeLast : boolean read FIncludeLast;
    property Female : boolean read FFemale;
    property Male : boolean read FMale;
    function GetField(const quoteChar : string = '') : string; override;
    function GetAsString : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string;
                       const first : boolean;
                       const last : boolean;
                       const female : boolean;
                       const male : boolean);
  end;
  
  TCityField = class(TField)
  public
    function GetField(const quoteChar : string = '') : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string);
  end;
  
  TStateField = class(TField)
  private
    FFull : boolean;
  public
    property Full : boolean read FFull;
    function GetField(const quoteChar : string = '') : string; override;
    function GetAsString : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string;
                       const full : boolean);
  end;
  
  TCountryField = class(TField)
  public
    function GetField(const quoteChar : string = '') : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string);
  end;
  
  TZipField = class(TField)
  public
    function GetField(const quoteChar : string = '') : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string);
  end;
  
  TPostcodeField = class(TField)
  public
    function GetField(const quoteChar : string = '') : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string);
  end;
  
  TAddressField = class(TField)
  public
    function GetField(const quoteChar : string = '') : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string);
  end;

  TEmailAddressField = class(TField)
  public
    function GetField(const quoteChar : string = '') : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string);
  end;
  
  TPhoneField = class(TField)
  public
    function GetField(const quoteChar : string = '') : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string);
  end;
  
  TSocSecField = class(TField)
  public
    function GetField(const quoteChar : string = '') : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string);
  end;
  
  TWordsField = class(TField)
  private
    FLow : longint;
    FHigh : longint;
  public
    property LowVal : longint read FLow;
    property HighVal : longint read FHigh;
    function GetField(const quoteChar : string = '') : string; override;
    function GetAsString : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string;
                       const low : longint;
                       const high : longint);
  end;
  
  TStringField = class(TField)
  private
    FLow : longint;
    FHigh : longint;
    FAllowAlpha : boolean;
    FAllowNumber : boolean;
    FAllowSpace : boolean;
    FAllowOther : boolean;
    FAllowedSet : TList;
  public
    property LowVal : longint read FLow;
    property HighVal : longint read FHigh;
    property AllowAlpha : boolean read FAllowAlpha;
    property AllowNumber : boolean read FAllowNumber;
    property AllowSpace : boolean read FAllowSpace;
    property AllowOther : boolean read FAllowOther;
    function GetField(const quoteChar : string = '') : string; override;
    function GetAsString : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string;
                       const low : longint;
                       const high : longint;
                       const alpha : boolean;
                       const numeric : boolean;
                       const space : boolean;
                       const other : boolean);
    destructor Destroy; override;
  end;
  
  TIPv4Field = class(TField)
  public
    function GetField(const quoteChar : string = '') : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string);
  end;

  TMacField = class(TField)
  public
    function GetField(const quoteChar : string = '') : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string);
  end;
  
  TGuidField = class(TField)
  private
    u : TGUID;
    tmp : string;
  public
    function GetField(const quoteChar : string = '') : string; override;
    constructor Create(const name : string; const theType : string; const theSubType : string);
  end;

// -Utility Routines -----------------------------------------------------------
{ return a random number from a range of integers }
function RandomRange(const low : longint;
                     const high : longint) : longint;
function FRandomRange(const low : double;
                      const high : double) : double;

{ Log a message to Syslog or to the Windows event log }
procedure LogSystemMessage (S : string);

const
  OUTPUT_TYPE_DELIMITED = 0;
  OUTPUT_TYPE_SQL = 1;
  OUTPUT_TYPE_MYSQL = 2;
  OUTPUT_TYPE_XML = 3;
  OUTPUT_TYPE_EXCEL = 4;

  
  STRING_ALPHA = 0;
  STRING_NUMBER = 1;
  STRING_SPACE = 2;
  STRING_OTHER = 3;
  
  SEX_MALE = 0;
  SEX_FEMALE = 1;

  TYPE_RANGE_NAME = 'Numbers';
  TYPE_HUMAN_NAME = 'Human';
  TYPE_TIME_NAME = 'Time';
  TYPE_TEXT_NAME = 'Text';
  TYPE_SET_NAME = 'Set';
  TYPE_GUID_NAME = 'GUID';
  TYPE_NET_NAME = 'Networking';

  SUBTYPE_NAME_NAME = 'Full Name';
  SUBTYPE_FIRSTNAME_NAME = 'First Name';
  SUBTYPE_LASTNAME_NAME = 'Last Name';
  SUBTYPE_EMAIL_NAME = 'E-Mail Address (name@domain.tld)';
  SUBTYPE_PHONE_NAME = 'Phone Number (###-###-####)';
  SUBTYPE_ADDRESS_NAME = 'Street Address';
  SUBTYPE_CITY_NAME = 'City';
  SUBTYPE_ZIP_NAME = 'US ZIP Code (#####)';
  SUBTYPE_POSTCODE_NAME = 'UK Postcode';
  SUBTYPE_STATE_NAME = 'State';
  SUBTYPE_COUNTRY_NAME = 'Country';
  SUBTYPE_DATE_NAME = 'Date (yyyy-mm-dd)';
  SUBTYPE_TIME_NAME = 'Time (HH:nn:ss)';
  SUBTYPE_DATETIME_NAME = 'Date/Time (yyyy-mm-dd HH:nn:ss)';
  SUBTYPE_SS_NAME = 'Social Security Number';

  SUBTYPE_FIXEDWORDS_NAME = 'Fixed Number of Words';
  SUBTYPE_RANDOMWORDS_NAME = 'Random Number of Words';
  SUBTYPE_FIXEDALPHA_NAME = 'Fixed-length String';
  SUBTYPE_RANDOMALPHA_NAME = 'Random-length String';

  SUBTYPE_INTEGERRANGE_NAME = 'Integer';
  SUBTYPE_REALRANGE_NAME = 'Real';
  SUBTYPE_SEQUENCE_NAME = 'Sequence';
  
  SUBTYPE_IP_NAME = 'IPv4 Address';
  SUBTYPE_MAC_NAME = 'MAC Address';

implementation

// -Field classes---------------------------------------------------------------

constructor TField.Create(const name : string; const theType : string; const theSubType : string);
begin
  FName := name;
  FType := theType;
  FSubType := theSubType;
  Randomize;
end;

function TField.GetField(const quoteChar : string = '') : string;
begin
  result := FName;
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

function TField.GetAsString : string;
begin
  result := FName + ',' + FType + ',' + FSubType;
end;

procedure TField.Reset;
begin
  // do nothing (stateless field)
end;

// -Integer range --------------------------------------------------------------
constructor TIntegerRangeField.Create(const name : string; const theType : string; const theSubType : string;
                                      const low : longint;
                                      const high : longint);
begin
  Inherited Create(name, theType, theSubType);
  FLow := low;
  FHigh := high;
end;

function TIntegerRangeField.GetField(const quoteChar : string = '') : string;
begin
  result := IntToStr(RandomRange(FLow, FHigh));
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

function TIntegerRangeField.GetAsString : string;
begin
  result := Inherited GetAsString + ',' + IntToStr(FLow) + '|' + IntToStr(FHigh);
end;

// -Real range -----------------------------------------------------------------
constructor TRealRangeField.Create(const name : string; const theType : string; const theSubType : string;
                                   const low : Double;
                                   const high : Double;
                                   const decimalPlaces : byte);
begin
  Inherited Create(name, theType, theSubType);
  FLow := low;
  FHigh := high;
  FDecimalPlaces := decimalPlaces;
end;

function TRealRangeField.GetField(const quoteChar : string = '') : string;
var
  theNumber : double;
begin
  result := FloatToStrF(FRandomRange(FLow, FHigh), ffFixed, 15, FDecimalPlaces);
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

function TRealRangeField.GetAsString : string;
begin
  result := Inherited GetAsString + ',' + FloatToStr(FLow) + '|' + FloatToStr(FHigh) + '|' + IntToStr(FDecimalPlaces);
end;

// -Date/Time range ------------------------------------------------------------
constructor TDateTimeRangeField.Create(const name : string; const theType : string; const theSubType : string;
                                       const low : TDateTime;
                                       const high : TDateTime;
                                       const includeDate : boolean;
                                       const includeTime : boolean;
                                       const currentOnly : boolean);
begin
  Inherited Create(name, theType, theSubType);
  FLow := low;
  FHigh := high;
  FCurrentOnly := currentOnly;
  FIncludeDate := includeDate;
  FIncludeTime := includeTime;
end;

function TDateTimeRangeField.GetField(const quoteChar : string = '') : string;
var
  theDateTime : TDateTime;
begin
  result := '';
  if (FCurrentOnly) then begin
    theDateTime := now;
  end else begin
    theDateTime := FRandomRange(FLow, FHigh);
  end;
  if (FIncludeDate) then begin
    result := FormatDateTime('yyyy-mm-dd', theDateTime);
  end;
  if (FIncludeTime) then begin
    if result <> '' then result := result + ' ';
    result := result + FormatDateTime('HH:nn:ss', theDateTime);
  end;
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

function TDateTimeRangeField.GetAsString : string;
begin
  result := Inherited GetAsString + ',' + FloatToStr(FLow) + '|' + FloatToStr(FHigh) + '|' + BoolToStr(FCurrentOnly);
end;

// -Set ------------------------------------------------------------------------
constructor TSetField.Create(const name : string; const theType : string; const theSubType : string;
                             const text : string );
begin
  Inherited Create(name, theType, theSubType);
  FSet := TStringList.Create;
  FSet.Text := text;
end;

destructor TSetField.Destroy;
begin
  if Assigned(FSet) then FreeAndNil(FSet);
  Inherited Destroy;
end;

function TSetField.GetSetString : string;
begin
  result := '';
  if Assigned(FSet) then result := FSet.Text;
end;

function TSetField.GetField(const quoteChar : string = '') : string;
var
  index : longint;
begin
  index := RandomRange(0, FSet.Count-1);
  result := FSet.Strings[index];
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

function TSetField.GetAsString : string;
var
  i : integer;
  otherPart : string;
begin
  result := Inherited GetAsString + ',';
  otherPart := '';
  for i := 0 to FSet.Count-1 do begin
    otherPart := otherPart + FSet.Strings[i];
    if (i < FSet.Count-1) then otherPart := otherPart + '|';
  end;
  result := result + otherPart;
end;

// -Sequence--------------------------------------------------------------------
constructor TSequenceField.Create(const name : string; const theType : string; const theSubType : string;
                                  const start : longint;
                                  const duplicate : longint;
                                  const stride : longint);
begin
  Inherited Create(name, theType, theSubType);
  FStart := start;
  FDuplicate := duplicate;
  FStride := stride;
  Reset;
end;

procedure TSequenceField.Reset;
begin
  if (FDuplicate = 0) then begin
    FLastNumber := FStart - FStride;
  end else begin
    FLastNumber := FStart;
  end;
  FCurrentDuplicate := 0;
end;

function TSequenceField.GetField(const quoteChar : string = '') : string;
var
  resultNumber : longint;
begin
  if (FDuplicate = 0) or (FCurrentDuplicate >= FDuplicate) then begin
    resultNumber := FLastNumber + FStride;
    FCurrentDuplicate := 0;
  end else begin
    resultNumber := FLastNumber;
    inc(FCurrentDuplicate);
  end;
  FLastNumber := resultNumber;
  result := IntToStr(FLastNumber);
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

function TSequenceField.GetAsString : string;
begin
  result := Inherited GetAsString + ',' + IntToStr(FStart) + '|' + IntToStr(FDuplicate) + '|' + IntToStr(FStride);
end;

// -Name -----------------------------------------------------------------------
constructor TNameField.Create(const name : string; const theType : string; const theSubType : string;
                              const first : boolean;
                              const last : boolean;
                              const female : boolean;
                              const male : boolean);
begin
  Inherited Create(name, theType, theSubType);
  FIncludeFirst := first;
  FIncludeLast := last;
  FFemale := female;
  FMale := male;
end;

function TNameField.GetField(const quoteChar : string = '') : string;
begin
  result := '';
  if (FIncludeFirst) then begin
    if (FFemale) and (FMale) then begin
      if (Random >= 0.5) then begin
        result := FemaleNames[RandomRange(Low(FemaleNames), High(FemaleNames))];
      end else begin
        result := MaleNames[RandomRange(Low(MaleNames), High(MaleNames))];
      end;
    end else if (FFemale) then begin
      result := FemaleNames[RandomRange(Low(FemaleNames), High(FemaleNames))];
    end else begin
      result := MaleNames[RandomRange(Low(MaleNames), High(MaleNames))];
    end;
  end;
  if (FIncludeLast) then begin
    if (result <> '') then result := result + ' ';
    result := result + LastNames[RandomRange(Low(LastNames), High(LastNames))];
  end;
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

function TNameField.GetAsString : string;
begin
  result := Inherited GetAsString + ',' + BoolToStr(FFemale) + '|' + BoolToStr(FMale);
end;

// -City -----------------------------------------------------------------------
constructor TCityField.Create(const name : string; const theType : string; const theSubType : string);
begin
  Inherited Create(name, theType, theSubType);
end;

function TCityField.GetField(const quoteChar : string = '') : string;
begin
  result := CityNames[RandomRange(Low(CityNames), High(CityNames))];
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;


// -State ----------------------------------------------------------------------
constructor TStateField.Create(const name : string; const theType : string; const theSubType : string;
                               const full : boolean);
begin
  Inherited Create(name, theType, theSubType);
  FFull := full;
end;

function TStateField.GetField(const quoteChar : string = '') : string;
begin
  if FFull then
    result := StateNames[RandomRange(Low(StateNames), High(StateNames))]
  else
    result := StateAbbreviations[RandomRange(Low(StateAbbreviations), High(StateAbbreviations))];
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

function TStateField.GetAsString : string;
begin
  result := Inherited GetAsString + ',' + BoolToStr(FFull);
end;

// -Country -----------------------------------------------------------------------
constructor TCountryField.Create(const name : string; const theType : string; const theSubType : string);
begin
  Inherited Create(name, theType, theSubType);
end;

function TCountryField.GetField(const quoteChar : string = '') : string;
begin
  result := CountryNames[RandomRange(Low(CountryNames), High(CountryNames))];
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

// -ZIP Code--------------------------------------------------------------------
constructor TZipField.Create(const name : string; const theType : string; const theSubType : string);
begin
  Inherited Create(name, theType, theSubType);
end;

function TZipField.GetField(const quoteChar : string = '') : string;
begin
  result := SysUtils.Format('%.*d', [5, RandomRange(0, 99999)]) ;
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

// -Postcode Code--------------------------------------------------------------------
constructor TPostcodeField.Create(const name : string; const theType : string; const theSubType : string);
begin
  Inherited Create(name, theType, theSubType);
end;

// UK postcodes take, in the main, the format
// ccnn ncc
// Where c is an upper case character, and n is a digit
function TPostcodeField.GetField(const quoteChar : string = '') : string;
begin
  result := chr(ord('A') + RandomRange(0, 25)) + chr(ord('A') + RandomRange(0, 25)) +
            SysUtils.Format('%d',[RandomRange(0, 99)]) + ' ' +
            SysUtils.Format('%d',[RandomRange(0, 9)]) +
           chr(ord('A') + RandomRange(0, 25)) + chr(ord('A') + RandomRange(0, 25));
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

// -Address --------------------------------------------------------------------
constructor TAddressField.Create(const name : string; const theType : string; const theSubType : string);
begin
  Inherited Create(name, theType, theSubType);
end;

function TAddressField.GetField(const quoteChar : string = '') : string;
var
  rVal : extended;
begin
  result := IntToStr(RandomRange(0, 99999));
  result := result + ' ' + Directions[RandomRange(Low(Directions), High(Directions))];
  rVal := Random;
  if (rVal > 0.8) then begin
    result := result + ' ' + CountryNames[RandomRange(Low(CountryNames), High(CountryNames))]
  end else if (rVal > 0.6) then begin
    result := result + ' ' + CountryNames[RandomRange(Low(LastNames), High(LastNames))]
  end else if (rVal > 0.4) then begin
    result := result + ' ' + CountryNames[RandomRange(Low(StateNames), High(StateNames))]
  end else if (rVal > 0.2) then begin
    result := result + ' ' + CountryNames[RandomRange(Low(CountryNames), High(CountryNames))];
  end else begin
    result := result + ' ' + CountryNames[RandomRange(Low(FemaleNames), High(FemaleNames))]
  end;
  result := result + ' ' + StreetTypes[RandomRange(Low(StreetTypes), High(StreetTypes))];
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

// -Email Address --------------------------------------------------------------
constructor TEmailAddressField.Create(const name : string; const theType : string; const theSubType : string);
begin
  Inherited Create(name, theType, theSubType);
end;

function TEmailAddressField.GetField(const quoteChar : string = '') : string;
var
  rVal : extended;
begin
  rVal := Random;
  if (rVal >= 0.5) then begin
    result := FemaleNames[RandomRange(Low(FemaleNames), High(FemaleNames))];
  end else begin
    result := MaleNames[RandomRange(Low(MaleNames), High(MaleNames))];
  end;
  result := result +
            '@' +
            LatinWords[RandomRange(Low(LatinWords), High(LatinWords))] +
            '.' +
            TopLevelDomains[RandomRange(Low(TopLevelDomains), High(TopLevelDomains))];
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

// -Phone Number ---------------------------------------------------------------
constructor TPhoneField.Create(const name : string; const theType : string; const theSubType : string);
begin
  Inherited Create(name, theType, theSubType);
end;

function TPhoneField.GetField(const quoteChar : string = '') : string;
begin
  result := SysUtils.Format('%.*d', [3, RandomRange(0, 999)]) + '-' +
            SysUtils.Format('%.*d', [3, RandomRange(0, 999)]) + '-' +
            SysUtils.Format('%.*d', [4, RandomRange(0, 9999)]);
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

// -Social Security Number -----------------------------------------------------
constructor TSocSecField.Create(const name : string; const theType : string; const theSubType : string);
begin
  Inherited Create(name, theType, theSubType);
end;

function TSocSecField.GetField(const quoteChar : string = '') : string;
begin
  result := SysUtils.Format('%.*d', [3, RandomRange(0, 999)]) + '-' +
            SysUtils.Format('%.*d', [2, RandomRange(0, 99)]) + '-' +
            SysUtils.Format('%.*d', [4, RandomRange(0, 9999)]);
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

// - Words ---------------------------------------------------------------------
constructor TWordsField.Create(const name : string; const theType : string; const theSubType : string;
                               const low : longint;
                               const high : longint);
begin
  Inherited Create(name, theType, theSubType);
  FLow := low;
  FHigh := high;
end;

function TWordsField.GetField(const quoteChar : string = '') : string;
var
  i : integer;
  numWords : integer;
begin
  result := '';
  
  if (FLow = FHigh) then
    numWords := FHigh
  else
    numWords := RandomRange(FLow, FHigh);
  
  for i := 0 to numWords-1 do begin
    if i > 0 then result := result + ' ';
    result := result + LatinWords[RandomRange(Low(LatinWords), High(LatinWords))];
  end;

  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

function TWordsField.GetAsString : string;
begin
  result := Inherited GetAsString + ',' + IntToStr(FLow) + '|' + IntToStr(FHigh);
end;

// - String --------------------------------------------------------------------
constructor TStringField.Create(const name : string; const theType : string; const theSubType : string;
                                const low : longint;
                                const high : longint;
                                const alpha : boolean;
                                const numeric : boolean;
                                const space : boolean;
                                const other : boolean);
var
  i : byte;
begin
  Inherited Create(name, theType, theSubType);
  FLow := low;
  FHigh := high;
  FAllowAlpha := alpha;
  FAllowNumber := numeric;
  FAllowSpace := space;
  FAllowOther := other;
  FAllowedSet := TList.Create;
  if alpha then begin
    for i := Ord('a') to Ord('z') do begin
      FAllowedSet.Add(Pointer(i))
    end;
    for i := Ord('A') to Ord('Z') do begin
      FAllowedSet.Add(Pointer(i))
    end;
  end;
  if numeric then begin
    for i := Ord('0') to Ord('9') do begin
      FAllowedSet.Add(Pointer(i))
    end;
  end;
  if space then begin
    FAllowedSet.Add(Pointer(Ord(' ')));
  end;
  if other then begin
    for i := Ord('!') to Ord('/') do begin
      FAllowedSet.Add(Pointer(i))
    end;
    for i := Ord(':') to Ord('@') do begin
      FAllowedSet.Add(Pointer(i))
    end;
    for i := Ord('[') to Ord('`') do begin
      FAllowedSet.Add(Pointer(i))
    end;
    for i := Ord('{') to Ord('~') do begin
      FAllowedSet.Add(Pointer(i))
    end;
    (* it's a pain to deal with these *)
    FAllowedSet.Delete(FAllowedSet.IndexOf(Pointer(Ord(''''))));
    FAllowedSet.Delete(FAllowedSet.IndexOf(Pointer(Ord('"'))));
  end;
end;

destructor TStringField.Destroy;
begin
  if Assigned(FAllowedSet) then FreeAndNil(FAllowedSet);
  Inherited Destroy;
end;

function TStringField.GetField(const quoteChar : string = '') : string;
var
  i : integer;
  numChars : integer;
  chosen : integer;
  index : longint;
begin
  result := '';
  
  if (FLow = FHigh) then
    numChars := FHigh
  else
    numChars := RandomRange(FLow, FHigh);
    
  for i := 0 to numChars-1 do begin
    index := RandomRange(0, FAllowedSet.Count-1);
    result := result + Chr(byte(FAllowedSet.Items[index]));
  end;

  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

function TStringField.GetAsString : string;
begin
  result := Inherited GetAsString + ',' + IntToStr(FLow) + '|' + IntToStr(FHigh) + '|' + BoolToStr(FAllowAlpha) + '|' + BoolToStr(FAllowNumber) + '|' + BoolToStr(FAllowSpace) + '|' + BoolToStr(FAllowOther);
end;

// -IPv4 Address ---------------------------------------------------------------
constructor TIPv4Field.Create(const name : string; const theType : string; const theSubType : string);
begin
  Inherited Create(name, theType, theSubType);
end;

function TIPv4Field.GetField(const quoteChar : string = '') : string;
begin
  result := IntToStr(RandomRange(0,255)) + '.' +
            IntToStr(RandomRange(0,255)) + '.' +
            IntToStr(RandomRange(0,255)) + '.' +
            IntToStr(RandomRange(0,255));
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

// -MAC Address ---------------------------------------------------------------
constructor TMacField.Create(const name : string; const theType : string; const theSubType : string);
begin
  Inherited Create(name, theType, theSubType);
end;

function TMacField.GetField(const quoteChar : string = '') : string;
begin
  result := IntToHex(RandomRange(0,255),2) + ':' +
            IntToHex(RandomRange(0,255),2) + ':' +
            IntToHex(RandomRange(0,255),2) + ':' +
            IntToHex(RandomRange(0,255),2) + ':' +
            IntToHex(RandomRange(0,255),2) + ':' +
            IntToHex(RandomRange(0,255),2);
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

// -GUID ------- ---------------------------------------------------------------
constructor TGUIDField.Create(const name : string; const theType : string; const theSubType : string);
begin
  Inherited Create(name, theType, theSubType);
end;

function TGUIDField.GetField(const quoteChar : string = '') : string;
begin
  CreateGuid(u);
  tmp := GuidToString(u);
  result := Copy(tmp, 2, 8) +
            Copy(tmp, 11, 4) +
            Copy(tmp, 16, 4) +
            Copy(tmp, 21, 4) +
            Copy(tmp, 26, 12);
  if (Length(quoteChar) > 0) then result := quoteChar + result + quoteChar;
end;

// -Utility Routines -----------------------------------------------------------
function RandomRange(const low : longint;
                     const high : longint) : longint;
begin
  if (high < low) then
    result := high + random(low - high + 1)
  else
    Result := low + random(high - low + 1);
end;

function FRandomRange(const low : double;
                      const high : double) : double;
begin
  if (high < low) then
    Result := high + Random * (low - high)
  else
    Result := low + Random * (high - low);
end;

procedure LogSystemMessage (S : string);
var
  T : string;
{$IFDEF MSWINDOWS}
  szStrings: Array[0..0] Of PChar;
  LogHandle : THandle;
begin
  try
    T := ExtractFileName(Application.ExeName);
    S := #$D#$A#$D#$A+S+#0;
    szStrings[0] := @S[1];
    LogHandle:= OpenEventLog(nil,PAnsiChar(T));
    if LogHandle <> 0 then begin
      ReportEvent(LogHandle,EVENTLOG_AUDIT_SUCCESS,0,0,nil,1,0,@szStrings,nil);
      CloseEventLog(LogHandle);
    end;
  except
  end;
{$ELSE}
begin
  try
    T := ExtractFileName(Application.ExeName);
    Libc.openlog(PChar(T),LOG_PID,LOG_LOCAL5);
    {$IFNDEF FPC}
    Libc.syslog(LOG_INFO,PChar(S));
    {$ELSE}
    Libc.syslog(LOG_INFO,PChar(S), []);
    {$ENDIF}
    Libc.closelog;
  except
  end;
{$ENDIF}
end;

end.

