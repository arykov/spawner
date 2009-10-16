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

unit main;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, LResources, LCLType, Forms, Controls,
  Graphics, Dialogs, ComCtrls,
  StdCtrls, Menus, mysql50conn, sqldb,
  Clipbrd, SynEdit, SynMemo, Buttons, ExtCtrls, Spin, RegExpr,
  EditBtn, common;

type

  { TMainForm }

  TMainForm = class(TForm)
    FieldNameEdit: TEdit;
    FieldOptionDateLowCalendar: TDateEdit;
    FieldOptionDateHighCalendar: TDateEdit;
    FieldOptionDateLowLabel: TLabel;
    FieldOptionDateHighLabel: TLabel;
    FieldOptionsSetFileEdit: TFileNameEdit;
    GenerationCancelButton: TBitBtn;
    GenerationProgressBar: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
    FieldNameLabel: TLabel;
    FieldOptionsMaskEdit : TLabeledEdit;
    FieldOptionsSetFileNameLabel: TLabel;
    MySQLSleepCheckbox: TCheckBox;
    ExitButton: TBitBtn;
    FieldOptionDateTimeUnixCheckBox: TCheckBox;
    GenerateButton: TBitBtn;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    LoadMenuItem: TMenuItem;
    BottomPanel: TPanel;
    ExecutionProgressPanel: TPanel;
    FieldOptionStepRadioGroup: TRadioGroup;
    FieldOptionsMaskPage : TPage;
    FieldOptionsSetFilePage: TPage;
    SaveMenuItem: TMenuItem;
    ClearAllMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    OutputFileChooseButton: TBitBtn;
    OutputFileLabel: TLabel;
    OutputFileNameEdit: TEdit;
    OutputMySQLErrorHaltCheckbox: TCheckBox;
    FieldsSaveDialog: TSaveDialog;
    FieldsLoadDialog : TOpenDialog;
    MySQLHostEdit: TLabeledEdit;
    MySQLUserEdit: TLabeledEdit;
    MySQLPasswordEdit: TLabeledEdit;
    MySQLDatabaseEdit: TLabeledEdit;
    OutputMySQLFileTooCheckBox: TCheckBox;
    OutputMySQLTableEdit: TLabeledEdit;
    OutputMySQLPage: TPage;
    OutputMySqlInsertOperationRadioGroup: TRadioGroup;
    OutputMySqlFieldNamesCheckbox: TCheckBox;
    OutputSqlRecordsPerInsertLabel1: TLabel;
    OutputMySqlRecordsPerInsertSpinEdit: TSpinEdit;
    CutMenuItem1: TMenuItem;
    OutputSqlFieldNamesCheckbox: TCheckBox;
    FieldOptionNameSexCheckGroup: TCheckGroup;
    FieldOptionSetAddItemButton: TBitBtn;
    FieldOptionFixedStringAllowedCheckGroup: TCheckGroup;
    FieldOptionRandomStringAllowedCheckGroup: TCheckGroup;
    FieldOptionFixedStringLabel: TLabel;
    FieldOptionFixedStringSpinEdit: TSpinEdit;
    FieldOptionFixedWordLabel: TLabel;
    FieldOptionFixedWordSpinEdit: TSpinEdit;
    FieldOptionIntegerRangeHighLabel: TLabel;
    FieldOptionIntegerRangeHighSpinEdit: TSpinEdit;
    FieldOptionIntegerRangeLowLabel: TLabel;
    FieldOptionIntegerRangeLowSpinEdit: TSpinEdit;
    FieldOptionRandomWordFromLabel: TLabel;
    FieldOptionRandomStringFromLabel: TLabel;
    FieldOptionRandomWordHighSpinEdit: TSpinEdit;
    FieldOptionRandomStringHighSpinEdit: TSpinEdit;
    FieldOptionRandomWordLowSpinEdit: TSpinEdit;
    FieldOptionRandomStringLowSpinEdit: TSpinEdit;
    FieldOptionRandomWordsLabel: TLabel;
    FieldOptionRandomStringLabel: TLabel;
    FieldOptionRandomWordToLabel: TLabel;
    FieldOptionRandomStringToLabel: TLabel;
    FieldOptionRealRangeHighLabel1: TLabel;
    FieldOptionRealRangeHighLabel2: TLabel;
    FieldOptionRealRangeHighSpinEdit: TFloatSpinEdit;
    FieldOptionRealRangeLowLabel1: TLabel;
    FieldOptionRealRangeLowSpinEdit: TFloatSpinEdit;
    FieldOptionsDateTimePage: TPage;
    FieldOptionsFixedAlphaPage: TPage;
    FieldOptionsFixedWordsPage: TPage;
    FieldOptionsGroupBox: TGroupBox;
    FieldOptionsNoneLabel: TLabel;
    FieldOptionsNonePage: TPage;
    FieldOptionsNotebook: TNotebook;
    FieldOptionsRandomAlphaPage: TPage;
    FieldOptionsRandomWordsPage: TPage;
    FieldOptionsRangeIntPage: TPage;
    FieldOptionsRangeRealPage: TPage;
    FieldOptionsSequencePage: TPage;
    FieldOptionTimeLowGroupBox: TGroupBox;
    FieldOptionTimeHighGroupBox: TGroupBox;
    FieldOptionTimeLowLabel: TLabel;
    FieldOptionTimeHighLabel: TLabel;
    FieldOptionSetRemoveItemButton: TBitBtn;
    FieldOptionTimeLowEdit: TEdit;
    FieldOptionTimeHighEdit: TEdit;
    FieldSaveButton: TBitBtn;
    FieldRemoveButton: TBitBtn;
    FieldSubTypeComboBox: TComboBox;
    FieldSubtypeLabel: TLabel;
    FieldTypeComboBox: TComboBox;
    FieldTypeGroupBox: TGroupBox;
    FieldTypeLabel: TLabel;
    FieldUpButton: TBitBtn;
    FieldAddButton: TBitBtn;
    FieldDownButton: TBitBtn;
    ClearMemoMenuItem: TMenuItem;
    FieldListBox: TListBox;
    FieldsDetailsGroupBox: TGroupBox;
    FieldsLabel: TLabel;
    FieldOptionSequenceStartLabel: TLabel;
    FieldOptionSequenceDupLabel: TLabel;
    FieldOptionSequenceStrideLabel: TLabel;
    FieldOptionSetLabel: TLabel;
    FieldOptionSetListBox: TListBox;
    OutputQuoteCharEdit: TLabeledEdit;
    OutputSqlRecordsPerInsertLabel: TLabel;
    OutputNumLabel: TLabel;
    OutputNotImplementedLabel: TLabel;
    MemoPopupMenu: TPopupMenu;
    MainPageControl: TPageControl;
    CutMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    OutputDelimitedPage: TPage;
    OutputDelimiterEdit: TLabeledEdit;
    OutputOptionsGroupBox: TGroupBox;
    OutputOptionsNotebook: TNotebook;
    OutputSaveDialog: TSaveDialog;
    OutputSqlInsertOperationRadioGroup: TRadioGroup;
    OutputSqlPage: TPage;
    OutputSqlTableNameEdit: TLabeledEdit;
    OutputTypeRadioGroup: TRadioGroup;
    OutputNotImplementedPage: TPage;
    FieldOptionsStatePage: TPage;
    FieldOptionStateAbbrRadioButton: TRadioButton;
    FieldOptionStateFullRadioButton: TRadioButton;
    FieldOptionsSetFixedPage: TPage;
    FieldOptionsNamePage: TPage;
    PasteMenuItem1: TMenuItem;
    SpacerPanel3: TPanel;
    SpacerPanel2: TPanel;
    SpacerPanel1: TPanel;
    SelectAllMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    OutputNumSpinEdit: TSpinEdit;
    FieldOptionSequenceStartSpinEdit: TSpinEdit;
    FieldOptionSequenceDupSpinEdit: TSpinEdit;
    FieldOptionSequenceStrideSpinEdit: TSpinEdit;
    FieldOptionRealRangeDecimalSpinEdit: TSpinEdit;
    OutputSqlRecordsPerInsertSpinEdit: TSpinEdit;
    MySQLSleepMsSpinEdit: TSpinEdit;
    MySQLSleepRecordsSpinEdit: TSpinEdit;
    FieldOptionsMaskHelpText : TStaticText;
    StatusBar: TStatusBar;
    MessagesMemo: TSynMemo;
    FieldsTabSheet: TTabSheet;
    MessagesTabSheet: TTabSheet;
    OutputTabSheet: TTabSheet;
    procedure ExitButtonClick(Sender: TObject);
    procedure FieldAddButtonClick(Sender: TObject);
    procedure FieldDownButtonClick(Sender: TObject);
    procedure FieldListBoxClick(Sender: TObject);
    procedure FieldListBoxDblClick(Sender: TObject);
    procedure FieldOptionSetAddItemButtonClick(Sender: TObject);
    procedure FieldOptionSetRemoveItemButtonClick(Sender: TObject);
    procedure FieldRemoveButtonClick(Sender: TObject);
    procedure FieldSaveButtonClick(Sender: TObject);
    procedure FieldSubTypeComboBoxSelect(Sender: TObject);
    procedure FieldTypeComboBoxSelect(Sender: TObject);
    procedure FieldUpButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure GenerateButtonClick(Sender: TObject);
    procedure GenerationCancelButtonClick(Sender: TObject);
    procedure MemoMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GlobalExceptionHandler(Sender:TObject; E:Exception);
    procedure OutputFileChooseButtonClick(Sender: TObject);
    procedure OutputTypeRadioGroupClick(Sender: TObject);
    procedure FieldOptionRealRangeDecimalSpinEditChange(Sender: TObject);
    procedure FileMenuItemClick(Sender: TObject);
  private
    FCancelled : boolean;
    procedure ShowString(const S : string; const Status : boolean = true);
    procedure SetStatus(const S : string);
    procedure SaveFields;
    procedure LoadFields;
    procedure ChangeComboBoxes(const theType : string;
                               const theSubType : string);
  public

  end; 

var
  MainForm: TMainForm;


implementation

{ TMainForm }

procedure TMainForm.MemoMenuItemClick(Sender: TObject);
var
  TheMemo : TSynMemo;
  TheItem : TMenuItem;
begin
  try
    TheMemo := nil;
    TheItem := nil;
    with Sender as TMenuItem do begin    { TMenuItem that clicked }
      TheItem := (Sender as TMenuItem);
      with Parent as TMenuItem do begin   { Items property of the TPopupMenu }
        with Owner as TPopupMenu do begin   { TPopupMenu }
          if PopupComponent is TSynMemo then begin
            TheMemo := (PopupComponent as TSynMemo);
          end;
        end;
      end;
    end;
    if Assigned(TheMemo) then begin
      if (TheItem = ClearMemoMenuItem) then begin
        TheMemo.Lines.Clear;
      end else if (TheItem = SelectAllMenuItem) then begin
        TheMemo.SelectAll;
      end else if (TheItem = CopyMenuItem) then begin
        ClipBoard.AsText := TheMemo.SelText;
      end else if (TheItem = CutMenuItem) then begin
        ClipBoard.AsText := TheMemo.SelText;
        // not done yet
      end else if (TheItem = PasteMenuItem) then begin
        // not done yet
      end
    end;
  except
  end;
end;

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FieldAddButtonClick(Sender: TObject);
var
  i : integer;
  theEvent : TNotifyEvent;
begin
  FCancelled := true;
  { set the field options to default and make a new name }
  theEvent := FieldListBox.OnClick;
  try
    FieldListBox.OnClick := nil;
    for i := 0 to FieldListBox.Items.Count-1 do begin
      FieldListBox.Selected[i] := false;
    end;
  finally
    FieldListBox.OnClick := theEvent;
  end;
  
  FieldNameEdit.Text := 'New Field';
  FieldTypeComboBox.ItemIndex := 0;
  FieldTypeComboBoxSelect(FieldTypeComboBox);
  FieldSubTypeComboBoxSelect(FieldSubTypeComboBox);
end;

procedure TMainForm.FieldDownButtonClick(Sender: TObject);
var
  tmpString : string;
  tmpObject : TObject;
begin
  { move the selected field down in order in the list }
  if (FieldListBox.ItemIndex > -1) and
     (FieldListBox.ItemIndex < FieldListBox.Items.Count-1) then begin
    tmpString := FieldListBox.Items.Strings[FieldListBox.ItemIndex];
    tmpObject := FieldListBox.Items.Objects[FieldListBox.ItemIndex];
    FieldListBox.Items.Strings[FieldListBox.ItemIndex] := FieldListBox.Items.Strings[FieldListBox.ItemIndex+1];
    FieldListBox.Items.Objects[FieldListBox.ItemIndex] := FieldListBox.Items.Objects[FieldListBox.ItemIndex+1];
    FieldListBox.Items.Strings[FieldListBox.ItemIndex+1] := tmpString;
    FieldListBox.Items.Objects[FieldListBox.ItemIndex+1] := tmpObject;
    FieldListBox.ItemIndex := FieldListBox.ItemIndex+1;
  end;
end;

procedure TMainForm.ChangeComboBoxes(const theType : string;
                                     const theSubType : string);
begin
  if (theType <> '') then begin
    FieldTypeComboBox.ItemIndex := FieldTypeComboBox.Items.IndexOf(theType);
    FieldTypeComboBoxSelect(FieldTypeComboBox);
    if (theSubType <> '') then begin
      FieldSubTypeComboBox.ItemIndex := FieldSubTypeComboBox.Items.IndexOf(theSubType);
      FieldSubTypeComboBoxSelect(FieldSubTypeComboBox);
    end;
  end else begin
    FieldTypeComboBox.ItemIndex := -1;
  end;
end;

procedure TMainForm.FieldListBoxClick(Sender: TObject);
var
  theObject : TObject;
  theField : TField;
begin
  if (FieldListBox.ItemIndex = -1) then exit;

  theObject := FieldListBox.Items.Objects[FieldListBox.ItemIndex];
  theField := (TField(theObject));

  if Assigned(theField) then begin
    // change the combo boxes and open the right notebook page
    ChangeComboBoxes(theField.ItemType, theField.ItemSubType);

    FieldNameEdit.Text := theField.Name;
    
    // set up whatever controls need to be set up
    if (theField is TSetField) then begin
      FieldOptionSetListBox.Items.Clear;
      FieldOptionsSetFileEdit.Clear;
      if ((theField as TSetField).FileName = '') then begin
        FieldOptionSetListBox.Items.Text := (theField as TSetField).GetSetString;
      end else begin
        FieldOptionsSetFileEdit.FileName := (theField as TSetField).FileName;
      end;
    end else if (theField is TIntegerRangeField) then begin
      FieldOptionIntegerRangeLowSpinEdit.Value := (theField as TIntegerRangeField).LowVal;
      FieldOptionIntegerRangeHighSpinEdit.Value := (theField as TIntegerRangeField).HighVal;
    end else if (theField is TRealRangeField) then begin
      FieldOptionRealRangeLowSpinEdit.Value := (theField as TRealRangeField).LowVal;
      FieldOptionRealRangeHighSpinEdit.Value := (theField as TRealRangeField).HighVal;
      FieldOptionRealRangeDecimalSpinEdit.Value := (theField as TRealRangeField).DecimalPlaces;
    end else if (theField is TSequenceField) then begin
      FieldOptionSequenceStartSpinEdit.Value := (theField as TSequenceField).Start;
      FieldOptionSequenceDupSpinEdit.Value := (theField as TSequenceField).Duplicate;
      FieldOptionSequenceStrideSpinEdit.Value := (theField as TSequenceField).Stride;
    end else if (theField is TStateField) then begin
      FieldOptionStateFullRadioButton.Checked := (theField as TStateField).Full;
    end else if (theField is TDateTimeRangeField) then begin
      FieldOptionDateTimeUnixCheckBox.Checked := (theField as TDateTimeRangeField).DisplayUnix;
      if ((theField as TDateTimeRangeField).TimeType = tsNow) then
        FieldOptionStepRadioGroup.ItemIndex := 0
      else if ((theField as TDateTimeRangeField).TimeType = tsIncrementing) then
        FieldOptionStepRadioGroup.ItemIndex := 1
      else if ((theField as TDateTimeRangeField).TimeType = tsDecrementing) then
        FieldOptionStepRadioGroup.ItemIndex := 2
      else
        FieldOptionStepRadioGroup.ItemIndex := 3;
      if (FieldOptionStepRadioGroup.ItemIndex = 0) then begin
        FieldOptionDateLowCalendar.Date := now-7;
        FieldOptionDateHighCalendar.Date := now;
        FieldOptionTimeLowEdit.Text := FormatDateTime('HH:nn:ss', now);
        FieldOptionTimeHighEdit.Text := FormatDateTime('HH:nn:ss', now);
      end else begin
        FieldOptionDateLowCalendar.Date := (theField as TDateTimeRangeField).LowVal;
        FieldOptionDateHighCalendar.Date := (theField as TDateTimeRangeField).HighVal;
        FieldOptionTimeLowEdit.Text := FormatDateTime('HH:nn:ss', (theField as TDateTimeRangeField).LowVal);
        FieldOptionTimeHighEdit.Text := FormatDateTime('HH:nn:ss', (theField as TDateTimeRangeField).HighVal);
      end;
    end else if (theField is TWordsField) then begin
      FieldOptionFixedWordSpinEdit.Value := (theField as TWordsField).LowVal;
      FieldOptionRandomWordLowSpinEdit.Value := (theField as TWordsField).LowVal;
      FieldOptionRandomWordHighSpinEdit.Value := (theField as TWordsField).HighVal;
    end else if (theField is TStringField) then begin
      FieldOptionFixedStringSpinEdit.Value := (theField as TStringField).LowVal;
      FieldOptionRandomStringLowSpinEdit.Value := (theField as TStringField).LowVal;
      FieldOptionRandomStringHighSpinEdit.Value := (theField as TStringField).HighVal;
      FieldOptionFixedStringAllowedCheckGroup.Checked[STRING_ALPHA] := (theField as TStringField).AllowAlpha;
      FieldOptionFixedStringAllowedCheckGroup.Checked[STRING_NUMBER] := (theField as TStringField).AllowNumber;
      FieldOptionFixedStringAllowedCheckGroup.Checked[STRING_SPACE] := (theField as TStringField).AllowSpace;
      FieldOptionFixedStringAllowedCheckGroup.Checked[STRING_OTHER] := (theField as TStringField).AllowOther;
      FieldOptionRandomStringAllowedCheckGroup.Checked[STRING_ALPHA] := (theField as TStringField).AllowAlpha;
      FieldOptionRandomStringAllowedCheckGroup.Checked[STRING_NUMBER] := (theField as TStringField).AllowNumber;
      FieldOptionRandomStringAllowedCheckGroup.Checked[STRING_SPACE] := (theField as TStringField).AllowSpace;
      FieldOptionRandomStringAllowedCheckGroup.Checked[STRING_OTHER] := (theField as TStringField).AllowOther;
    end else if (theField is TNameField) then begin
      FieldOptionNameSexCheckGroup.Checked[SEX_FEMALE] := (theField as TNameField).Female;
      FieldOptionNameSexCheckGroup.Checked[SEX_MALE] := (theField as TNameField).Male;
    end else if (theField is TMaskField) then begin
      FieldOptionsMaskEdit.Text := (theField as TMaskField).Mask;
    end;
  end;
end;

procedure TMainForm.FieldListBoxDblClick(Sender: TObject);
var
  theObject : TObject;
  theField : TField;
begin
  if (FieldListBox.ItemIndex > -1) then begin
    theObject := FieldListBox.Items.Objects[FieldListBox.ItemIndex];
    theField := (TField(theObject));
    if (theField is TSequenceField) then begin
      SetStatus(theField.Name);
      exit; // sequence has state
    end;
    if Assigned(theField) then begin
      SetStatus(theField.Name + ' data example: "' + theField.GetField + '"');
    end else begin
      ShowString('Selected field ' +
                 FieldListBox.Items.Strings[FieldListBox.ItemIndex] +
                 ' was not created correctly!');
    end;
  end;
end;

procedure TMainForm.FieldOptionSetAddItemButtonClick(Sender: TObject);
var
  newString : string;
begin
  { add an item to the set list }
  newString :=   InputBox ('New Set Value',
                           'Enter a new value to be included in the set:',
                           '');
  if (trim(newString) <> '') then  FieldOptionSetListBox.Items.Add(newString);
end;

procedure TMainForm.FieldOptionSetRemoveItemButtonClick(Sender: TObject);
begin
  { remove an item from the set list }
  if (FieldOptionSetListBox.ItemIndex >= 0) then begin
    FieldOptionSetListBox.Items.Delete(FieldOptionSetListBox.ItemIndex);
  end;
end;

procedure TMainForm.FieldRemoveButtonClick(Sender: TObject);
var
  deleteReply : integer;
  theObject : TObject;
  theField : TField;
  needPrompt : boolean;
begin
  needPrompt := true;
  if (Sender is TMenuItem) and ((Sender as TMenuItem) = ClearAllMenuItem) then needPrompt := false;
  { delete the selected field from the list }
  if (FieldListBox.ItemIndex > -1) then begin
    if (needPrompt) and (FieldListBox.Count > 0) then begin
      deleteReply :=  Application.MessageBox (PChar('Remove field ' +  FieldListBox.Items.Strings[FieldListBox.ItemIndex] + '?'),
                                                 'Remove?', MB_ICONQUESTION + MB_YESNO);
    end else begin
      deleteReply := IDYES;
    end;
    if (deleteReply = IDYES) then begin
      theObject := FieldListBox.Items.Objects[FieldListBox.ItemIndex];
      theField := (TField(theObject));
      ShowString('Deleting field ' + theField.Name + ' of type/subtype ' +
                 theField.ItemType + ' ' + theField.ItemSubType);
      if Assigned(theField) then FreeAndNil(theField);
      FieldListBox.Items.Delete(FieldListBox.ItemIndex);
    end else begin
      exit;
    end;
  end;
end;

procedure TMainForm.FieldSaveButtonClick(Sender: TObject);
var
  overwriteReply : integer;
  errMsg : string;
  theOldField: TField;
  theNewField : TField;
  theObject : TObject;
  lowTime, highTime : TDateTime;
  timeType : TTimeSequence;
  oldPosition : integer;
begin
  { check the input and save the field }
  FieldNameEdit.Text := trim(FieldNameEdit.Text);
  oldPosition := -1;
  if (FieldNameEdit.Text = '') then begin
    ShowString('A name must be defined for the new field');
    exit;
  end;
  if (FieldListBox.Items.IndexOf(FieldNameEdit.Text) > -1) then begin
    overwriteReply :=  Application.MessageBox (PChar('Overwrite current settings for ' +  FieldNameEdit.Text + '?'),
                                               'Overwrite?', MB_ICONQUESTION + MB_YESNO);
    if overwriteReply = IDYES then begin
      { delete the existing field and its object }
      theObject := FieldListBox.Items.Objects[FieldListBox.Items.IndexOf(FieldNameEdit.Text)];
      theOldField := (TField(theObject));
      if Assigned(theOldField) then FreeAndNil(theOldField);
      oldPosition := FieldListBox.Items.IndexOf(FieldNameEdit.Text);
      FieldListBox.Items.Delete(FieldListBox.Items.IndexOf(FieldNameEdit.Text));
     end else begin
       exit;
     end;
  end;
  
  { do input checking and whatnot }
  errMsg := '';
  theNewField := nil;
  try
    if (FieldSubTypeComboBox.Text = SUBTYPE_SET_FIXED) then begin
      if (FieldOptionSetListBox.Items.Count <= 0) then begin
        errMsg := 'A set must contain at least one item';
      end else begin
        theNewField := TSetField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text, false, FieldOptionSetListBox.Items.Text);
      end;
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_SET_FILE) then begin
      if not FileExists(FieldOptionsSetFileEdit.FileName) then begin
        errMsg := 'Could not create set: file does not exist';
      end else begin
        theNewField := TSetField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text, true, FieldOptionsSetFileEdit.FileName);
      end;
    end else if (FieldTypeComboBox.Text = TYPE_GUID_NAME) then begin
      theNewField := TGuidField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text);
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_INTEGERRANGE_NAME) then begin
      theNewField := TIntegerRangeField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                               FieldOptionIntegerRangeLowSpinEdit.Value,
                                               FieldOptionIntegerRangeHighSpinEdit.Value);
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_REALRANGE_NAME) then begin
      theNewField := TRealRangeField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                            FieldOptionRealRangeLowSpinEdit.Value,
                                            FieldOptionRealRangeHighSpinEdit.Value,
                                            FieldOptionRealRangeDecimalSpinEdit.Value);
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_SEQUENCE_NAME) then begin
      theNewField := TSequenceField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                           FieldOptionSequenceStartSpinEdit.Value,
                                           FieldOptionSequenceDupSpinEdit.Value,
                                           FieldOptionSequenceStrideSpinEdit.Value);
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_STATE_NAME) then begin
      theNewField := TStateField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                        FieldOptionStateFullRadioButton.Checked);
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_DATE_NAME) then begin
      try
        lowTime := trunc(FieldOptionDateLowCalendar.Date);
        highTime := trunc(FieldOptionDateHighCalendar.Date);
        if (FieldOptionStepRadioGroup.ItemIndex = 0) then
          timeType := tsNow
        else if (FieldOptionStepRadioGroup.ItemIndex = 1) then
          timeType := tsIncrementing
        else if (FieldOptionStepRadioGroup.ItemIndex = 2) then
          timeType := tsDecrementing
        else
          timeType := tsRandom;
        theNewField := TDateTimeRangeField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                                  lowTime,
                                                  highTime,
                                                  true,
                                                  false,
                                                  timeType,
                                                  FieldOptionDateTimeUnixCheckBox.Checked);
      except
        on E : Exception do begin
          errMsg := E.Message;
        end;
      end;
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_TIME_NAME) then begin
      try
        lowTime := trunc(now) + Frac(StrToTime(FieldOptionTimeLowEdit.Text));
        highTime := trunc(now) + Frac(StrToTime(FieldOptionTimeHighEdit.Text));
        if (FieldOptionStepRadioGroup.ItemIndex = 0) then
          timeType := tsNow
        else if (FieldOptionStepRadioGroup.ItemIndex = 1) then
          timeType := tsIncrementing
        else if (FieldOptionStepRadioGroup.ItemIndex = 2) then
          timeType := tsDecrementing
        else
          timeType := tsRandom;
        theNewField := TDateTimeRangeField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                                  lowTime,
                                                  highTime,
                                                  false,
                                                  true,
                                                  timeType,
                                                  FieldOptionDateTimeUnixCheckBox.Checked);
      except
        on E : Exception do begin
          errMsg := E.Message;
        end;
      end;
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_DATETIME_NAME) then begin
      try
        lowTime := trunc(FieldOptionDateLowCalendar.Date) + Frac(StrToTime(FieldOptionTimeLowEdit.Text));
        highTime := trunc(FieldOptionDateHighCalendar.Date) + Frac(StrToTime(FieldOptionTimeHighEdit.Text));
        if (FieldOptionStepRadioGroup.ItemIndex = 0) then
          timeType := tsNow
        else if (FieldOptionStepRadioGroup.ItemIndex = 1) then
          timeType := tsIncrementing
        else if (FieldOptionStepRadioGroup.ItemIndex = 2) then
          timeType := tsDecrementing
        else
          timeType := tsRandom;
        theNewField := TDateTimeRangeField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                                  lowTime,
                                                  highTime,
                                                  true,
                                                  true,
                                                  timeType,
                                                  FieldOptionDateTimeUnixCheckBox.Checked);
      except
        on E : Exception do begin
          errMsg := E.Message;
        end;
      end;
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_FIXEDWORDS_NAME) then begin
      theNewField := TWordsField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                        FieldOptionFixedWordSpinEdit.Value,
                                        FieldOptionFixedWordSpinEdit.Value);
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_RANDOMWORDS_NAME) then begin
      theNewField := TWordsField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                        FieldOptionRandomWordLowSpinEdit.Value,
                                        FieldOptionRandomWordHighSpinEdit.Value);
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_FIXEDALPHA_NAME) then begin
      theNewField := TStringField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                         FieldOptionFixedStringSpinEdit.Value,
                                         FieldOptionFixedStringSpinEdit.Value,
                                         FieldOptionFixedStringAllowedCheckGroup.Checked[STRING_ALPHA],
                                         FieldOptionFixedStringAllowedCheckGroup.Checked[STRING_NUMBER],
                                         FieldOptionFixedStringAllowedCheckGroup.Checked[STRING_SPACE],
                                         FieldOptionFixedStringAllowedCheckGroup.Checked[STRING_OTHER]);
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_RANDOMALPHA_NAME) then begin
      theNewField := TStringField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                         FieldOptionRandomStringLowSpinEdit.Value,
                                         FieldOptionRandomStringHighSpinEdit.Value,
                                         FieldOptionRandomStringAllowedCheckGroup.Checked[STRING_ALPHA],
                                         FieldOptionRandomStringAllowedCheckGroup.Checked[STRING_NUMBER],
                                         FieldOptionRandomStringAllowedCheckGroup.Checked[STRING_SPACE],
                                         FieldOptionRandomStringAllowedCheckGroup.Checked[STRING_OTHER]);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_MASK_NAME) then begin
      theNewField := TMaskField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text, FieldOptionsMaskEdit.Text);
    end else if (FieldSubTypeComboBox.Text = SUBTYPE_NAME_NAME) then begin
      theNewField := TNameField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                       true,
                                       true,
                                       FieldOptionNameSexCheckGroup.Checked[SEX_FEMALE],
                                       FieldOptionNameSexCheckGroup.Checked[SEX_MALE]);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_FIRSTNAME_NAME) then begin
      theNewField := TNameField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                       true,
                                       false,
                                       FieldOptionNameSexCheckGroup.Checked[SEX_FEMALE],
                                       FieldOptionNameSexCheckGroup.Checked[SEX_MALE]);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_LASTNAME_NAME) then begin
      theNewField := TNameField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text,
                                       false,
                                       true,
                                       FieldOptionNameSexCheckGroup.Checked[SEX_FEMALE],
                                       FieldOptionNameSexCheckGroup.Checked[SEX_MALE]);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_EMAIL_NAME) then begin
      theNewField := TEmailAddressField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_PHONE_NAME) then begin
      theNewField := TPhoneField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_ADDRESS_NAME) then begin
      theNewField := TAddressField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_CITY_NAME) then begin
      theNewField := TCityField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_ZIP_NAME) then begin
      theNewField := TZipField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_POSTCODE_NAME) then begin
      theNewField := TPostcodeField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_COUNTRY_NAME) then begin
      theNewField := TCountryField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_SS_NAME) then begin
      theNewField := TSocSecField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_IP_NAME) then begin
      theNewField := TIPv4Field.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text);
    end else if( FieldSubTypeComboBox.Text = SUBTYPE_MAC_NAME) then begin
      theNewField := TMacField.Create(FieldNameEdit.Text, FieldTypeComboBox.Text, FieldSubTypeComboBox.Text);
    end else begin
      errMsg := 'Unknown type/subtype';
    end;
  except
    on E : Exception do begin
      errMsg := 'Error saving field: ' + E.Message;
    end;
  end;
  
  if (errMsg <> '') or (theNewField = nil) then begin
    ShowString(errMsg);
    exit;
  end;

  if (oldPosition >= 0) then begin
    FieldListBox.Items.InsertObject(oldPosition, theNewField.Name, theNewField);
  end else begin
    FieldListBox.Items.AddObject(theNewField.Name, theNewField);
  end;
  
  ShowString('Adding field ' + theNewField.Name + ' of type/subtype ' +
             theNewField.ItemType + ' ' + theNewField.ItemSubType);
end;

procedure TMainForm.FieldSubTypeComboBoxSelect(Sender: TObject);
var
  i : integer;
begin
  if (FieldSubTypeComboBox.Text = SUBTYPE_SET_FIXED) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsSetFixedPage;
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_SET_FILE) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsSetFilePage;
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_INTEGERRANGE_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsRangeIntPage;
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_REALRANGE_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsRangeRealPage;
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_SEQUENCE_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsSequencePage;
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_STATE_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsStatePage;
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_DATE_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsDateTimePage;
    FieldOptionDateLowCalendar.Enabled := true;
    FieldOptionDateHighCalendar.Enabled := true;
    FieldOptionDateLowCalendar.Date := now-7;
    FieldOptionDateHighCalendar.Date := now;
    FieldOptionTimeLowEdit.Enabled := false;
    FieldOptionTimeHighEdit.Enabled := false;
    FieldOptionTimeLowEdit.Text := '';
    FieldOptionTimeHighEdit.Text := '';
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_TIME_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsDateTimePage;
    FieldOptionDateLowCalendar.Enabled := false;
    FieldOptionDateHighCalendar.Enabled := false;
    FieldOptionDateLowCalendar.Date := now-7;
    FieldOptionDateHighCalendar.Date := now;
    FieldOptionTimeLowEdit.Enabled := true;
    FieldOptionTimeHighEdit.Enabled := true;
    FieldOptionTimeLowEdit.Text := '00:00:00';
    FieldOptionTimeHighEdit.Text := FormatDateTime('HH:nn:ss', now);
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_DATETIME_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsDateTimePage;
    FieldOptionDateLowCalendar.Enabled := true;
    FieldOptionDateHighCalendar.Enabled := true;
    FieldOptionDateLowCalendar.Date := now-7;
    FieldOptionDateHighCalendar.Date := now;
    FieldOptionTimeLowEdit.Enabled := true;
    FieldOptionTimeHighEdit.Enabled := true;
    FieldOptionTimeLowEdit.Text := '00:00:00';
    FieldOptionTimeHighEdit.Text := FormatDateTime('HH:nn:ss', now);
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_FIXEDWORDS_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsFixedWordsPage;
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_RANDOMWORDS_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsRandomWordsPage;
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_FIXEDALPHA_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsFixedAlphaPage;
    for i := 0 to FieldOptionFixedStringAllowedCheckGroup.Items.Count-1 do begin
      FieldOptionFixedStringAllowedCheckGroup.Checked[i] := true;
    end;
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_RANDOMALPHA_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsRandomAlphaPage;
    for i := 0 to FieldOptionRandomStringAllowedCheckGroup.Items.Count-1 do begin
      FieldOptionRandomStringAllowedCheckGroup.Checked[i] := true;
    end;
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_MASK_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsMaskPage;
    FieldOptionsMaskEdit.Text := '';
  end else if (FieldSubTypeComboBox.Text = SUBTYPE_NAME_NAME) or
              (FieldSubTypeComboBox.Text = SUBTYPE_FIRSTNAME_NAME) then begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsNamePage;
    for i := 0 to FieldOptionNameSexCheckGroup.Items.Count-1 do begin
      FieldOptionNameSexCheckGroup.Checked[i] := true;
    end;
  end else begin
    FieldOptionsNotebook.ActivePageComponent := FieldOptionsNonePage;
  end;
end;

procedure TMainForm.FieldTypeComboBoxSelect(Sender: TObject);
begin
  FieldSubTypeComboBox.Clear;
  FieldOptionsNotebook.ActivePageComponent := FieldOptionsNonePage;
  if (FieldTypeComboBox.Text = TYPE_RANGE_NAME) then begin
    FieldSubTypeComboBox.AddItem(SUBTYPE_INTEGERRANGE_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_REALRANGE_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_SEQUENCE_NAME, nil);
  end else if (FieldTypeComboBox.Text = TYPE_TIME_NAME) then begin
    FieldSubTypeComboBox.AddItem(SUBTYPE_DATE_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_TIME_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_DATETIME_NAME, nil);
  end else if (FieldTypeComboBox.Text = TYPE_HUMAN_NAME) then begin
    FieldSubTypeComboBox.AddItem(SUBTYPE_NAME_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_FIRSTNAME_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_LASTNAME_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_EMAIL_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_PHONE_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_ADDRESS_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_CITY_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_STATE_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_ZIP_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_POSTCODE_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_COUNTRY_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_SS_NAME, nil);
  end else if (FieldTypeComboBox.Text = TYPE_TEXT_NAME) then begin
    FieldSubTypeComboBox.AddItem(SUBTYPE_FIXEDWORDS_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_RANDOMWORDS_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_FIXEDALPHA_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_RANDOMALPHA_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_MASK_NAME, nil);
  end else if (FieldTypeComboBox.Text = TYPE_SET_NAME) then begin
    FieldSubTypeComboBox.AddItem(SUBTYPE_SET_FIXED, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_SET_FILE, nil);
  end else if (FieldTypeComboBox.Text = TYPE_NET_NAME) then begin
    FieldSubTypeComboBox.AddItem(SUBTYPE_IP_NAME, nil);
    FieldSubTypeComboBox.AddItem(SUBTYPE_MAC_NAME, nil);
  end;
  if (FieldSubTypeComboBox.Items.Count > 0) then FieldSubTypeComboBox.ItemIndex := 0;
  FieldSubTypeComboBoxSelect(FieldSubTypeComboBox);
end;

procedure TMainForm.FieldUpButtonClick(Sender: TObject);
var
  tmpString : string;
  tmpObject : TObject;
begin
  { move the selected field up in order in the list }
  if (FieldListBox.ItemIndex > 0) then begin
    tmpString := FieldListBox.Items.Strings[FieldListBox.ItemIndex];
    tmpObject := FieldListBox.Items.Objects[FieldListBox.ItemIndex];
    FieldListBox.Items.Strings[FieldListBox.ItemIndex] := FieldListBox.Items.Strings[FieldListBox.ItemIndex-1];
    FieldListBox.Items.Objects[FieldListBox.ItemIndex] := FieldListBox.Items.Objects[FieldListBox.ItemIndex-1];
    FieldListBox.Items.Strings[FieldListBox.ItemIndex-1] := tmpString;
    FieldListBox.Items.Objects[FieldListBox.ItemIndex-1] := tmpObject;
    FieldListBox.ItemIndex := FieldListBox.ItemIndex-1;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  reply, boxStyle: integer;
begin
  CanClose := false;
  with Application do begin
    boxStyle :=  MB_ICONQUESTION + MB_YESNO;
    reply :=  MessageBox ('Are you sure you want to exit?', 'Exit?', boxStyle);
  end;
  if reply = IDYES then CanClose := true;
end;

procedure TMainForm.GenerateButtonClick(Sender: TObject);
var
  i : longint;
  j : longint;
  modvalue : longint;
  currentLine : string;
  outputFile : TextFile;
  outputType : integer;
  theField : TField;
  delimiter : string;
  sqlBeginLine : string;
  currentSqlValueNumber : longint;
  sqlValuesPerInsert : longint;
  startTime : TDateTime;
  totalRecords : longint;
  opRadioGroup : TRadioGroup;
  opTableEdit : TLabeledEdit;
  opFieldCheckBox : TCheckBox;
  
  MySQLInsertConnection : TMySQL50Connection;
  SQLInsertTransaction : TSQLTransaction;
  SQLInsert : TSQLQuery;

  sleepEnabled : boolean;
  sleepMSecs : integer;
  sleepRecs : integer;
begin
  { generate the data according to the specified parameters }
  if (FieldListBox.Items.Count <= 0) then begin
    ShowString('There must be at least one field defined to generate data');
    exit;
  end else if (OutputOptionsNotebook.ActivePageComponent = OutputNotImplementedPage) then begin
    ShowString('The output method has not yet been implemented');
    exit;
  end;
  
  outputType := OutputTypeRadioGroup.ItemIndex;
  delimiter := OutputDelimiterEdit.Text;
  
  sleepEnabled := false;
  sleepMSecs := 0;
  sleepRecs := 0;

  if (outputType = OUTPUT_TYPE_MYSQL) then begin
    opRadioGroup := OutputMySqlInsertOperationRadioGroup;
    opTableEdit := OutputMySQLTableEdit;
    opFieldCheckBox := OutputMySqlFieldNamesCheckbox;
    sleepEnabled := MySQLSleepCheckbox.Checked;
    sleepMSecs := MySQLSleepMsSpinEdit.Value;
    sleepRecs := MySQLSleepRecordsSpinEdit.Value;
  end else begin
    OutputFileNameEdit.Text := trim(OutputFileNameEdit.Text);
    if FileExists(OutputFileNameEdit.Text) then begin
      if Application.MessageBox ('File exists. Overwrite?',
                                 'Overwrite?', MB_ICONQUESTION + MB_YESNO) = IDNO then exit;
    end;
    opRadioGroup := OutputSqlInsertOperationRadioGroup;
    opTableEdit := OutputSQLTableNameEdit;
    opFieldCheckBox := OutputSqlFieldNamesCheckbox;
  end;

  { for sql output mode, initialize the way our insert statements will begin
    and prepare for the first record which will be generated }
  sqlBeginLine := '';
  if (outputType = OUTPUT_TYPE_SQL) or (outputType = OUTPUT_TYPE_MYSQL) then begin
    sqlBeginLine := opRadioGroup.Items.Strings[opRadioGroup.ItemIndex] +
                    ' INTO ' + opTableEdit.Text;
    if (opFieldCheckBox.Checked) then begin
      sqlBeginLine := sqlBeginLine + ' (';
      for j := 0 to FieldListBox.Items.Count-1 do begin
        theField := TField(FieldListBox.Items.Objects[j]);
        sqlBeginLine := sqlBeginLine + theField.Name;
        if (j < FieldListBox.Items.Count-1) then sqlBeginLine := sqlBeginLine + ',';
      end;
      sqlBeginLine := sqlBeginLine + ')';
    end;
    sqlBeginLine := sqlBeginLine + ' VALUES ';
    currentLine := sqlBeginLine + '(';
  end;
  currentSqlValueNumber := 1;
  sqlValuesPerInsert := OutputSqlRecordsPerInsertSpinEdit.Value;

  totalRecords := OutputNumSpinEdit.Value;
  ShowString('Beginning generation of ' + IntToStr(totalRecords) + ' records to ' + OutputFileNameEdit.Text);
  try
    // get controls related to indicating status read for the data generation
    GenerateButton.Enabled := false;
    GenerationCancelButton.Visible := true;
    GenerationProgressBar.Visible := true;
    GenerationProgressBar.Min := 0;
    GenerationProgressBar.Max := OutputNumSpinEdit.Value-1;
    GenerationProgressBar.Position := 0;
    if (OutputNumSpinEdit.Value >= 100) then begin
      modvalue := OutputNumSpinEdit.Value div 100;
    end else begin
      modvalue := 1;
    end;

    if (outputType = OUTPUT_TYPE_MYSQL) then begin
      // prepare the mysql connection
      MySQLInsertConnection := TMySQL50Connection.Create(nil);
      SQLInsertTransaction := TSQLTransaction.Create(nil);
      SQLInsertTransaction.DataBase := MySQLInsertConnection;
      SQLInsert := TSQLQuery.Create(nil);
      SQLInsert.DataBase := MySQLInsertConnection;
      SQLInsert.Transaction := SQLInsertTransaction;
      SQLInsert.ParseSQL := false;

      try
        MySQLInsertConnection.DatabaseName := MySQLDatabaseEdit.Text;
        MySQLInsertConnection.HostName := MySQLHostEdit.Text;
        MySQLInsertConnection.UserName := MySqlUserEdit.Text;
        MySQLInsertConnection.Password := MySqlPasswordEdit.Text;
        ShowString('Opening a connection to server for insertions: ' + MySQLHostEdit.Text);
        MySQLInsertConnection.Open;
        if MySQLInsertConnection.Connected then begin
          ShowString('Connected to server: ' + MySQLHostEdit.Text);
        end;
      except
        on E : Exception do begin
          ShowString('Error connecting to server: ' + E.Message);
          exit;
        end;
      end;
    end else begin

    end;
    if (outputType <> OUTPUT_TYPE_MYSQL) or (OutputMySQLFileTooCheckBox.Checked) then begin
      // prepare the output file
      AssignFile(outputFile, OutputFileNameEdit.Text);
    end;
    try
      if (outputType <> OUTPUT_TYPE_MYSQL) or (OutputMySQLFileTooCheckBox.Checked) then ReWrite(outputFile);

      // reset the fields if needed
      for j := 0 to FieldListBox.Items.Count-1 do begin
        theField := TField(FieldListBox.Items.Objects[j]);
        theField.Reset(totalRecords);
      end;

      // iterate for the number of records to generate
      startTime := now;
      for i := 0 to OutputNumSpinEdit.Value-1 do begin

        // process MessagesTabSheet and update status
        if FCancelled then break;
        if ((i mod modvalue) = 0) or
           ((i mod 20000) = 0) then begin
          if ((i mod modvalue) = 0) then GenerationProgressBar.Position := i;
          if (totalRecords > 0) and (i > 0) then begin
            SetStatus('Generated ' + IntToStr(i) + ' of ' +
                      IntToStr(totalRecords) + ' records, ETA ' +
                      FormatDateTime('HH:nn:ss', ((100 * (now - startTime) / (i * 100/ totalRecords)) - (now - startTime))));
          end;
            Application.ProcessMessages;
        end;

        // loop through each field in the field list
        for j := 0 to FieldListBox.Items.Count-1 do begin

          theField := TField(FieldListBox.Items.Objects[j]);

          if (outputType = OUTPUT_TYPE_DELIMITED) then begin
            // regular character-delimited output
            currentLine := currentLine + theField.GetField(OutputQuoteCharEdit.Text);
            if (j < FieldListBox.Items.Count-1) then currentLine := currentLine + delimiter;

          end else if (outputType = OUTPUT_TYPE_SQL) or (outputType = OUTPUT_TYPE_MYSQL) then begin
            // sql insert statement formatted output (each value enclosed in
            // single quotes and comma-separated)
            currentLine := currentLine + theField.GetField('''');
            if (j < FieldListBox.Items.Count-1) then currentLine := currentLine + ',';

          end; // end output type checks
        end; // end fields for loop

        if (outputType = OUTPUT_TYPE_SQL) or (outputType = OUTPUT_TYPE_MYSQL) then begin
          { terminate the current record for sql inserts, and output to the
            file if the number of records per insert has been reached }
          currentLine := currentLine + ')';
          if (currentSqlValueNumber >= sqlValuesPerInsert) then begin
            // it's time to write the current statement
            currentLine := currentLine + ';';
            currentSqlValueNumber := 1;
            if (outputType = OUTPUT_TYPE_MYSQL) then begin
              SQLInsert.SQL.Text := currentLine;
              try
                SQLInsert.ExecSQL;
              except
                on E : Exception do begin
                  if OutputMySQLErrorHaltCheckbox.Checked then begin
                    ShowString('Insert failed: ' + currentLine);
                    ShowString('Error while inserting: ' + E.Message);
                    exit;
                  end;
                end;
              end;
            end;
            if (outputType <> OUTPUT_TYPE_MYSQL) or (OutputMySQLFileTooCheckBox.Checked) then begin
              writeln(outputFile, currentLine);
            end;
            currentLine := sqlBeginLine + '(';    // prepare for the next record
          end else begin
            // it's not time to write to the file yet, so prep for the next one
            currentLine := currentLine + ',(';
            inc(currentSqlValueNumber);
          end;

        end else begin
          if (outputType = OUTPUT_TYPE_MYSQL) then begin
            SQLInsert.SQL.Text := currentLine;
            try
              SQLInsert.ExecSQL;
            except
              on E : Exception do begin
                if OutputMySQLErrorHaltCheckbox.Checked then begin
                  ShowString('Insert failed: ' + currentLine);
                  ShowString('Error while inserting: ' + E.Message);
                  exit;
                end;
              end;
            end;
          end;
          if (outputType <> OUTPUT_TYPE_MYSQL) or (OutputMySQLFileTooCheckBox.Checked) then begin
            // one record per line, so output the line and prep for the next one
            writeln(outputFile, currentLine);
          end;
          currentLine := '';
        end;
        
        if sleepEnabled and
           (outputType = OUTPUT_TYPE_MYSQL) and
           (i > 0) and
           (i mod sleepRecs = 0)
        then begin
          sleep(sleepMSecs);
        end;

      end; // end records for loop

      // output residual values of SQL insert statements, if any
      if ((outputType = OUTPUT_TYPE_SQL) or (outputType = OUTPUT_TYPE_MYSQL)) and
         (currentSqlValueNumber > 1) then begin
        if (currentLine[Length(currentLine)-1] = ',') and
           ((currentLine[Length(currentLine)] = '(')) then begin
          currentLine := Copy(currentLine, 1, Length(currentLine)-2);
        end;
        currentLine := currentLine + ';';
        if (outputType = OUTPUT_TYPE_MYSQL) then begin
          SQLInsert.SQL.Text := currentLine;
          try
            SQLInsert.ExecSQL;
          except
            on E : Exception do begin
              if OutputMySQLErrorHaltCheckbox.Checked then begin
                ShowString('Insert failed: ' + currentLine);
                ShowString('Error while inserting: ' + E.Message);
              end;
            end;
          end;
        end;
        if (outputType <> OUTPUT_TYPE_MYSQL) or (OutputMySQLFileTooCheckBox.Checked) then begin
          writeln(outputFile, currentLine);
        end;
      end;

    finally
      if (outputType = OUTPUT_TYPE_MYSQL) then begin
        if Assigned(SQLInsertTransaction) then SQLInsertTransaction.Active := False;
        try
          ShowString('Closing MySQL connection...');
          if Assigned(MySQLInsertConnection) and MySQLInsertConnection.Connected then MySQLInsertConnection.Close;
          ShowString('MySQL connection closed');
        except
          on E : Exception do begin
            ShowString('Error disconnecting from database: ' + E.Message);
          end;
        end;
        if Assigned(SQLInsert) then FreeAndNil(SQLInsert);
        if Assigned(SQLInsertTransaction) then FreeAndNil(SQLInsertTransaction);
        if Assigned(MySQLInsertConnection) then FreeAndNil(MySQLInsertConnection);
      end;
      if (outputType <> OUTPUT_TYPE_MYSQL) or (OutputMySQLFileTooCheckBox.Checked) then begin
        CloseFile(outputFile);
      end;
    end;
    if (outputType = OUTPUT_TYPE_MYSQL) then begin
      ShowString('Generated ' + IntToStr(i + 1) + ' records to ' + MySQLHostEdit.Text + ',' + MySQLDatabaseEdit.Text + ',' + OutputMySQLTableEdit.Text);
    end else begin
      ShowString('Generated ' + IntToStr(i + 1) + ' records to ' + OutputFileNameEdit.Text);
    end;
  finally
    GenerateButton.Enabled := true;
    GenerationCancelButton.Visible := false;
    GenerationProgressBar.Visible := false;
    FCancelled := false;
  end;
end;

procedure TMainForm.GenerationCancelButtonClick(Sender: TObject);
begin
  FCancelled := true;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnException := GlobalExceptionHandler;
  
  MainPageControl.ActivePage := FieldsTabSheet;
  
  FieldTypeComboBox.Clear;
  FieldTypeComboBox.AddItem(TYPE_RANGE_NAME, nil);
  FieldTypeComboBox.AddItem(TYPE_HUMAN_NAME, nil);
  FieldTypeComboBox.AddItem(TYPE_TIME_NAME, nil);
  FieldTypeComboBox.AddItem(TYPE_TEXT_NAME, nil);
  FieldTypeComboBox.AddItem(TYPE_SET_NAME, nil);
  FieldTypeComboBox.AddItem(TYPE_GUID_NAME, nil);
  FieldTypeComboBox.AddItem(TYPE_NET_NAME, nil);
  FieldTypeComboBox.ItemIndex := 0;
  FieldTypeComboBoxSelect(FieldTypeComboBox);
  FieldSubTypeComboBoxSelect(FieldSubTypeComboBox);
  
  OutputTypeRadioGroupClick(OutputTypeRadioGroup);
  
  FCancelled := false;
  
{$IFDEF Windows}
  OutputFileNameEdit.Text := 'C:\datagen.txt';
{$ELSE}
  OutputFileNameEdit.Text := '/tmp/datagen.txt';
{$ENDIF}

end;

procedure TMainForm.GlobalExceptionHandler(Sender:Tobject; E:Exception);
begin
  LogSystemMessage('Exception: ' + E.Message);
  try
    ShowString('Error: ' + E.Message);
  except
  end;
end;


procedure TMainForm.OutputFileChooseButtonClick(Sender: TObject);
begin
  if DirectoryExists(ExtractFileDir(trim(OutputFileNameEdit.Text))) then begin
    OutputSaveDialog.InitialDir := ExtractFilePath(trim(OutputFileNameEdit.Text));
  end;
  
  if OutputSaveDialog.Execute then begin
    OutputFileNameEdit.Text := OutputSaveDialog.FileName;
  end;
end;

procedure TMainForm.OutputTypeRadioGroupClick(Sender: TObject);
begin
  if ((Sender as TRadioGroup).ItemIndex = OUTPUT_TYPE_DELIMITED) then begin
    OutputOptionsNotebook.ActivePageComponent := OutputDelimitedPage;
  end else if ((Sender as TRadioGroup).ItemIndex = OUTPUT_TYPE_SQL) then begin
    OutputOptionsNotebook.ActivePageComponent := OutputSqlPage;
  end else if ((Sender as TRadioGroup).ItemIndex = OUTPUT_TYPE_MYSQL) then begin
    OutputOptionsNotebook.ActivePageComponent := OutputMySqlPage;
  end else begin
    OutputOptionsNotebook.ActivePageComponent := OutputNotImplementedPage;
  end;
end;

procedure TMainForm.FieldOptionRealRangeDecimalSpinEditChange(Sender: TObject);
begin
  FieldOptionRealRangeLowSpinEdit.DecimalPlaces := (Sender as TSpinEdit).Value;
  FieldOptionRealRangeHighSpinEdit.DecimalPlaces := (Sender as TSpinEdit).Value;
end;

procedure TMainForm.FileMenuItemClick(Sender: TObject);
var
  TheItem : TMenuItem;
  deleteReply : integer;
begin
  try
    TheItem := nil;
    with Sender as TMenuItem do begin    { TMenuItem that clicked }
      TheItem := (Sender as TMenuItem);
    end;
    if (TheItem = SaveMenuItem) then begin
      SaveFields;
    end else if (TheItem = LoadMenuItem) then begin
      LoadFields;
    end else if (TheItem = ClearAllMenuItem) then begin
      { delete all fields from the list }
      deleteReply :=  Application.MessageBox (PChar('Remove all fields?'),
                                                 'Remove?', MB_ICONQUESTION + MB_YESNO);
      if deleteReply = IDYES then begin
        while FieldListBox.Count > 0 do begin
          FieldListBox.ItemIndex := FieldListBox.Count-1;
          FieldRemoveButtonClick(TheItem);
        end;
      end;
    end else if (TheItem = QuitMenuItem) then begin
      ExitButton.Click;
    end;
  except
  end;
end;

procedure TMainForm.SaveFields;
var
  saveFileName : string;
  saveContents : TStringList;
  i : integer;
  theField : TField;
begin
  if FieldsSaveDialog.Execute then begin
    saveContents := TStringList.Create;
    try
      saveFileName := FieldsSaveDialog.FileName;
      for i := 0 to FieldListBox.Count-1 do begin
        theField := TField(FieldListBox.Items.Objects[i]);
        saveContents.Add(theField.GetAsString);
      end;
      saveContents.SaveToFile(saveFileName);
      ShowString('Saved table definition to ' + saveFileName);
    finally
      FreeAndNil(saveContents);
    end;
  end;
end;

procedure TMainForm.LoadFields;
var
  loadContents : TStringList;
  overwriteReply : integer;
  i : integer;
  name : string;
  fieldType : string;
  fieldSubType : string;
  itemsStringList : TStringList;
  otherStringList : TStringList;
  otherRegEx : TRegExpr;
begin
  if (FieldListBox.Count > 0) then begin
    overwriteReply :=  Application.MessageBox (PChar('This will replace all current fields. Continue?'),
                                               'Overwrite?', MB_ICONQUESTION + MB_YESNO);
  end else begin
    overwriteReply := IDYES;
  end;
  if (overwriteReply = IDYES) and FieldsLoadDialog.Execute then begin
    while FieldListBox.Count > 0 do begin
      FieldListBox.ItemIndex := FieldListBox.Count-1;
      FieldRemoveButtonClick(ClearAllMenuItem);
    end;
    
    loadContents := TStringList.Create;
    otherStringList := TStringList.Create;
    otherRegEx := TRegExpr.Create;
    itemsStringList := TStringList.Create;
    try
      otherRegEx.Expression := '|';
      loadContents.LoadFromFile(FieldsLoadDialog.FileName);
      for i := 0 to loadContents.Count-1 do begin
      
        otherRegEx.Expression := ',';
        itemsStringList.Clear;
        otherRegEx.Split(loadContents.Strings[i], itemsStringList);

        name := '';
        fieldType := '';
        fieldSubType := '';
        if (itemsStringList.Count >= 3) then begin
          name := itemsStringList.Strings[0];
          fieldType := itemsStringList.Strings[1];
          fieldSubType := itemsStringList.Strings[2];
        end;
        
        if (name <> '') and
           (fieldType <> '') and
           ((fieldType = TYPE_SET_NAME) or (fieldType = TYPE_GUID_NAME) or (fieldSubType <> '')) then begin
           
          otherStringList.Clear;
          if (itemsStringList.Count >= 4) then begin
            otherStringList.Delimiter := '|';
            otherStringList.DelimitedText := itemsStringList.Strings[3];
          end;

          (* create a new field according to the info given *)
          ChangeComboBoxes(fieldType, fieldSubType);
          fieldNameEdit.Text := name;
          
          (* populate the gui from the current line *)
          if (fieldType = TYPE_SET_NAME) and (fieldSubType = SUBTYPE_SET_FIXED) then begin
            FieldOptionSetListBox.Items.Text := otherStringList.Text;
          end else if (fieldType = TYPE_SET_NAME) and (fieldSubType = SUBTYPE_SET_FILE) then begin
            if (otherStringList.Count >= 1) then begin
              FieldOptionsSetFileEdit.FileName := otherStringList.Strings[0];
              if not FileExists(FieldOptionsSetFileEdit.FileName) then begin
                ShowString(FieldOptionsSetFileEdit.FileName + ' does not exist, skipping field ' + name);
                continue;
              end;
            end;
          end else if (fieldSubType = SUBTYPE_INTEGERRANGE_NAME) then begin
            if (otherStringList.Count >= 2) then begin
              FieldOptionIntegerRangeLowSpinEdit.Value := StrToIntDef(otherStringList.Strings[0], 0);
              FieldOptionIntegerRangeHighSpinEdit.Value := StrToIntDef(otherStringList.Strings[1], 0);
            end;
          end else if (fieldSubType = SUBTYPE_REALRANGE_NAME) then begin
            if (otherStringList.Count >= 3) then begin
              FieldOptionRealRangeDecimalSpinEdit.Value := StrToIntDef(otherStringList.Strings[2], 0);
              FieldOptionRealRangeLowSpinEdit.Value := StrToFloatDef(otherStringList.Strings[0], 0);
              FieldOptionRealRangeHighSpinEdit.Value := StrToFloatDef(otherStringList.Strings[1], 0);
            end;
          end else if (fieldSubType = SUBTYPE_SEQUENCE_NAME) then begin
            if (otherStringList.Count >= 3) then begin
              FieldOptionSequenceStartSpinEdit.Value := StrToIntDef(otherStringList.Strings[0], 0);
              FieldOptionSequenceDupSpinEdit.Value := StrToIntDef(otherStringList.Strings[1], 0);
              FieldOptionSequenceStrideSpinEdit.Value := StrToIntDef(otherStringList.Strings[2], 0);
            end;
          end else if (fieldSubType = SUBTYPE_STATE_NAME) then begin
            if (otherStringList.Count >= 1) then begin
              FieldOptionStateFullRadioButton.Checked := StrToBoolDef(otherStringList.Strings[0], false);
            end;
          end else if (fieldSubType = SUBTYPE_DATE_NAME) then begin
            if (otherStringList.Count >= 3) then begin
              FieldOptionDateLowCalendar.Date := StrToFloatDef(otherStringList.Strings[0], 0);
              FieldOptionDateHighCalendar.Date := StrToFloatDef(otherStringList.Strings[1], 0);
              FieldOptionTimeLowEdit.Text := '00:00:00';
              FieldOptionTimeHighEdit.Text := '00:00:00';
              FieldOptionStepRadioGroup.ItemIndex := StrToIntDef(otherStringList.Strings[2], 0);
              FieldOptionDateTimeUnixCheckBox.Checked := StrToBoolDef(otherStringList.Strings[3], false);
            end;
          end else if (fieldSubType = SUBTYPE_TIME_NAME) then begin
            if (otherStringList.Count >= 3) then begin
              FieldOptionDateLowCalendar.Date := trunc(now);
              FieldOptionDateHighCalendar.Date := trunc(now);
              FieldOptionTimeLowEdit.Text := FormatDateTime('HH:nn:ss', StrToFloatDef(otherStringList.Strings[0], 0));
              FieldOptionTimeHighEdit.Text := FormatDateTime('HH:nn:ss', StrToFloatDef(otherStringList.Strings[1], 0));
              FieldOptionStepRadioGroup.ItemIndex := StrToIntDef(otherStringList.Strings[2], 0);
              FieldOptionDateTimeUnixCheckBox.Checked := StrToBoolDef(otherStringList.Strings[3], false);
            end;
          end else if (fieldSubType = SUBTYPE_DATETIME_NAME) then begin
            if (otherStringList.Count >= 3) then begin
              FieldOptionDateLowCalendar.Date := StrToFloatDef(otherStringList.Strings[0], 0);
              FieldOptionDateHighCalendar.Date := StrToFloatDef(otherStringList.Strings[1], 0);
              FieldOptionTimeLowEdit.Text := FormatDateTime('HH:nn:ss', StrToFloatDef(otherStringList.Strings[0], 0));
              FieldOptionTimeHighEdit.Text := FormatDateTime('HH:nn:ss', StrToFloatDef(otherStringList.Strings[1], 0));
              FieldOptionStepRadioGroup.ItemIndex := StrToIntDef(otherStringList.Strings[2], 0);
              FieldOptionDateTimeUnixCheckBox.Checked := StrToBoolDef(otherStringList.Strings[3], false);
            end;
          end else if (fieldSubType = SUBTYPE_FIXEDWORDS_NAME) then begin
            if (otherStringList.Count >= 1) then begin
              FieldOptionFixedWordSpinEdit.Value := StrToIntDef(otherStringList.Strings[0], 0);
            end;
          end else if (fieldSubType = SUBTYPE_RANDOMWORDS_NAME) then begin
            if (otherStringList.Count >= 2) then begin
              FieldOptionRandomWordLowSpinEdit.Value := StrToIntDef(otherStringList.Strings[0], 0);
              FieldOptionRandomWordHighSpinEdit.Value := StrToIntDef(otherStringList.Strings[1], 0);
            end;
          end else if (fieldSubType = SUBTYPE_FIXEDALPHA_NAME) then begin
            if (otherStringList.Count >= 6) then begin
              FieldOptionFixedStringSpinEdit.Value := StrToIntDef(otherStringList.Strings[0], 0);
              FieldOptionFixedStringAllowedCheckGroup.Checked[STRING_ALPHA] := StrToBoolDef(otherStringList.Strings[2], true);
              FieldOptionFixedStringAllowedCheckGroup.Checked[STRING_NUMBER] := StrToBoolDef(otherStringList.Strings[3], true);
              FieldOptionFixedStringAllowedCheckGroup.Checked[STRING_SPACE] := StrToBoolDef(otherStringList.Strings[4], true);
              FieldOptionFixedStringAllowedCheckGroup.Checked[STRING_OTHER] := StrToBoolDef(otherStringList.Strings[5], true);
            end;
          end else if (fieldSubType = SUBTYPE_RANDOMALPHA_NAME) then begin
            if (otherStringList.Count >= 6) then begin
              FieldOptionRandomStringLowSpinEdit.Value := StrToIntDef(otherStringList.Strings[0], 0);
              FieldOptionRandomStringLowSpinEdit.Value := StrToIntDef(otherStringList.Strings[1], 0);
              FieldOptionRandomStringAllowedCheckGroup.Checked[STRING_ALPHA] := StrToBoolDef(otherStringList.Strings[2], true);
              FieldOptionRandomStringAllowedCheckGroup.Checked[STRING_NUMBER] := StrToBoolDef(otherStringList.Strings[3], true);
              FieldOptionRandomStringAllowedCheckGroup.Checked[STRING_SPACE] := StrToBoolDef(otherStringList.Strings[4], true);
              FieldOptionRandomStringAllowedCheckGroup.Checked[STRING_OTHER] := StrToBoolDef(otherStringList.Strings[5], true);
            end
          end else if (fieldSubType = SUBTYPE_MASK_NAME) then begin
            if (otherStringList.Count >= 1) then begin
              FieldOptionsMaskEdit.Text := otherStringList.Strings[0];
            end;
          end else if (fieldSubType = SUBTYPE_NAME_NAME) or
                      (fieldSubType = SUBTYPE_FIRSTNAME_NAME) then begin
            if (otherStringList.Count >= 2) then begin
              FieldOptionNameSexCheckGroup.Checked[SEX_FEMALE] := StrToBoolDef(otherStringList.Strings[0], true);
              FieldOptionNameSexCheckGroup.Checked[SEX_MALE] := StrToBoolDef(otherStringList.Strings[1], true);
            end;
          end else if (fieldType =    TYPE_GUID_NAME) or
                      (fieldSubType = SUBTYPE_LASTNAME_NAME) or
                      (fieldSubType = SUBTYPE_EMAIL_NAME) or
                      (fieldSubType = SUBTYPE_PHONE_NAME) or
                      (fieldSubType = SUBTYPE_ADDRESS_NAME) or
                      (fieldSubType = SUBTYPE_CITY_NAME) or
                      (fieldSubType = SUBTYPE_ZIP_NAME) or
                      (fieldSubType = SUBTYPE_POSTCODE_NAME) or
                      (fieldSubType = SUBTYPE_COUNTRY_NAME) or
                      (fieldSubType = SUBTYPE_SS_NAME) or
                      (fieldSubType = SUBTYPE_IP_NAME) or
                      (fieldSubType = SUBTYPE_MAC_NAME) then begin
            (* no configuration, do nothing *)
          end else begin
            (* unknown field type/subtype *)
            ShowString('[' + name + '] is unknown field type/subtype [' + fieldType + '] and [' + fieldSubType + ']');
            continue;
          end;
          
          (* actually create the new field *)
          FieldSaveButtonClick(FieldSaveButton);
          
        end else begin
          ShowString('Line [' + loadContents.Strings[i] + '] is invalid');
        end;
      end;
      
      ShowString('Loaded table definition from ' + FieldsLoadDialog.FileName);
      
    finally
      freeAndNil(loadContents);
      freeAndNil(otherStringList);
      freeAndNil(otherRegEx);
      freeAndNil(itemsStringList);
    end;
  end;
end;

procedure TMainForm.ShowString(const S : string;
                               const Status : boolean = true);
begin
  MessagesMemo.Lines.Add(TimeToStr(now) + ': ' + trim(S));
  MessagesMemo.SelStart := Length(MessagesMemo.Lines.Text)-1;
  if (Status) then begin
    SetStatus(S);
  end else begin
    Application.ProcessMessages;
  end;
end;

procedure TMainForm.SetStatus(const S : string);
begin
  StatusBar.SimpleText := S;
  Application.ProcessMessages;
end;

initialization
  {$I main.lrs}

end.

