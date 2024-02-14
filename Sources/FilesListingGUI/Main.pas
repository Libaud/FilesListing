unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus;

type

  { TMainF }

  TMainF = class(TForm)
    MainIL: TImageList;
    ListView1: TListView;
    MainM: TMainMenu;
    FilesMI: TMenuItem;
    HAboutMI: TMenuItem;
    AboutMi: TMenuItem;
    SearchMI: TMenuItem;
    CloseMI: TMenuItem;
    MainPM: TPopupMenu;
    Separator1: TMenuItem;
    MainS: TStatusBar;
    MainT: TToolBar;
    CloseTB: TToolButton;
    procedure AboutMiClick(Sender: TObject);
    procedure CloseMIClick(Sender: TObject);
    procedure SearchMIClick(Sender: TObject);
  private

  public

  end;

var
  MainF: TMainF;

implementation

{$R *.frm}

uses
  ListFiles, ListFilesUtilities;

{ TMainF }

procedure TMainF.SearchMIClick(Sender: TObject);
begin

end;

procedure TMainF.CloseMIClick(Sender: TObject);
begin
  Close;
end;

procedure TMainF.AboutMiClick(Sender: TObject);
begin

end;

end.

