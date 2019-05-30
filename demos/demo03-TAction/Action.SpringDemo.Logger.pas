{ * ------------------------------------------------------------------------
  * ♥  Bogdan Polak © 2019  ♥
  *  ----------------------------------------------------------------------- * }
unit Action.SpringDemo.Logger;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,
  Plus.Vcl.Timer,
  Action.Console;

type
  TSpringLoggerAction = class (TConsoleAction)
    procedure DoInitialiaze; override;
    procedure DoExecute; override;
  end;

implementation

// ------------------------------------------------------------------------
// ------------------------------------------------------------------------
// Demo: Simple construction of the ILogger interface
// * TLoggerController as ILoggerController
// * TFileLogAppender as ILogAppender
// -----------------------------------------------------------------------
// More info about Spring4D logging system and better way of usint loggers:
// * https://groups.google.com/forum/#!topic/spring4d/0E6GX-cfVrU
// * TLoggingConfiguration.LoadFromString - using Spring Container
// TODO: Prepare example with TLoggingConfigurationBuilder and configuration
// -----------------------------------------------------------------------
// Loger functionality comments:
// Subject: Clearing the log file
// * The log file is cleared between an app restarts
// * https://stackoverflow.com/questions/43150531/logger-implementation-to-use-in-spring4d
// -----------------------------------------------------------------------
// ------------------------------------------------------------------------

uses
  Spring,
  Spring.Logging,
  Spring.Logging.Loggers,
  Spring.Logging.Appenders,
  Spring.Logging.Controller;


procedure TSpringLoggerAction.DoInitialiaze;
begin
  Caption := 'Spring4D Logger in action';
end;

procedure TSpringLoggerAction.DoExecute;
var
  Controller: ILoggerController;
  Logger: ILogger;
  Appender: ILogAppender;
  Exception1: EAbort;
  sl: TStringList;
  LogFileName: string;
begin
  LogFileName := 'spring4d.txt';
  // --------------------------------------------------------------
  Controller := Spring.Logging.Controller.TLoggerController.Create;
  Logger := Spring.Logging.Loggers.TLogger.Create(Controller);
  Appender := Spring.Logging.Appenders.TFileLogAppender.Create;
  with Appender as TFileLogAppender do
  begin
    Levels := [TLogLevel.Warn, TLogLevel.Info, TLogLevel.Text, TLogLevel.Fatal];
    FileName := LogFileName;
  end;
  Controller.AddAppender(Appender);
  // --------------------------------------------------------------
  Logger.Enter(Self.ClassType, 'actLoggerDemoExecute');
  Logger.Log('first message');
  Logger.Warn('first WARN message');
  Exception1 := EAbort.Create('');
  Logger.Error('Error ... (not sent to appender)', Exception1);
  Exception1.Free;
  sl := TStringList.Create;
  try
    try
      sl[1] := 'abc';
    except
      on e: EStringListError do
        Logger.Fatal(e.Message, e);
    end;
  finally
    sl.Free;
  end;
  Logger.Leave(Self.ClassType, 'actLoggerDemoExecute');
  // --------------------------------------------------------------
  ConsoleWrite('---- Spring4D Logger-------------------------');
  ConsoleWrite('Log written to file: ' + LogFileName);
  TTimerPlus.RunOnce(Self, 1,
    procedure
    begin
      ConsoleWrite('---- log file -----------------------------');
      ConsoleWrite(System.IOUtils.TFile.ReadAllText(LogFileName));
      ConsoleWrite('---- -------- -----------------------------');
    end);
end;

end.
