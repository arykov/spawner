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

program datagen;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Classes,
  SysUtils,
  main, common, names;

{$R *.res}

var
  errMsg : ansistring;
begin
  // quick check parameters
  errMsg := Application.CheckOptions('hvi:o:n:t:d:q:', 'help verbose input: output: count: output-type: ' +
                                                       'delimiter: quote: quote-all field-names ' +
                                                       'sql-table-name: sql-recs-per-insert: sql-operation: ' +
                                                       'mysql-host: mysql-user: mysql-password: mysql-database: ' +
                                                       'mysql-halt-on-error mysql-file-output ' +
                                                       'mysql-sleep-freq: mysql-sleep-ms: ');
  if (errMsg <> '') then begin
    raise Exception.Create(errMsg);
  end;

  if Application.HasOption('h', 'help') then begin
    writeln(stderr, '');
    writeln(stderr, 'Command-line usage: ', ExtractFileName(Application.ExeName));
    writeln(stderr, ' -h, --help');
    writeln(stderr, '');
    writeln(stderr, ' -i, --input=<field list definition filename>');
    writeln(stderr, ' -o, --output=<output filename>');
    writeln(stderr, ' -n, --count=<number of records to output>');
    writeln(stderr, ' -t, --output-type=<delimited|fixed|sql|mysql>');
    writeln(stderr, ' -d, --delimiter=<delimiter character for delmited output>');
    writeln(stderr, ' -q, --quote=<quote character>');
    writeln(stderr, '     --quote-all (if specified, all fields will be quoted, not just alpha)');
    writeln(stderr, '     --field-names (if specified, include field names in header row or INSERT statements)');
    writeln(stderr, '     --sql-table-name=<table name if doing SQL insert statements>');
    writeln(stderr, '     --sql-recs-per-insert=<number of records per SQL insert statement>');
    writeln(stderr, '     --sql-operation=<SQL operation: INSERT, INSERT IGNORE, or REPLACE>');
    writeln(stderr, '     --mysql-host=<host if inserting via MySQL database connection>');
    writeln(stderr, '     --mysql-user=<user if inserting via MySQL database connection>');
    writeln(stderr, '     --mysql-password=<password if inserting via MySQL database connection>');
    writeln(stderr, '     --mysql-database=<database name if inserting via MySQL database connection>');
    writeln(stderr, '     --mysql-halt-on-error (in MySQL mode, halt if an error is encountered)');
    writeln(stderr, '     --mysql-file-output (in MySQL mode, also write INSERT statments to output file)');
    writeln(stderr, '     --mysql-sleep-freq=<in MySQL mode, sleep this often (# of records)>');
    writeln(stderr, '     --mysql-sleep-ms=<in MySQL mode, sleep this many milliseconds>');
    writeln(stderr, '');
    exit;
  end;

  Application.Title := 'Spawner Data Generator';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

