unit pgchart.test.view;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.DateUtils;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses pgchart;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  TPGChart.New.Gantt
    .AddRow('Atividade1', 'Atividade 01', 'Máquina 1', IncMinute(Now, -20), Now, 0, 100, '')
    .AddRow('Atividade2', 'Atividade 02', 'Máquina 2', IncMinute(Now, -40), Now, 0, 100, '')
    .AddRow('Atividade3', 'Atividade 03', 'Máquina 3', IncMinute(Now, -50), Now, 0, 100, '')
    .AddRow('Atividade4', 'Atividade 04', 'Máquina 3', IncMinute(Now, -10), Now, 0, 80, 'Atividade3')
    .AddRow('Atividade5', 'Atividade 05', 'Máquina 5', IncMinute(Now, -30), Now, 0, 25, '')
    .AddRow('Atividade6', 'Atividade 06', 'Máquina 1', 0, 0, 150000, 100, 'Atividade1')
    .AddOption('heigth', '560')
    .AddOption('width', '1024')
    .AddOption('gantt', '{defaultStartDate: new Date(2021, 1, 1)}')
    .Show;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TPGChart.New.Bar
    .AddLabels(['Ano', 'Vendas', 'Despesas', 'Lucro'])
    .AddValues(['2014', 1000, 400, 200])
    .AddValues(['2015', 1170, 460, 250])
    .AddValues(['2016', 660, 1120, 300])
    .AddValues(['2017', 1030, 540, 350])
    .AddOption('height', '560')
    .AddOption('width', '1024')
    .AddOption('chart', '{title: ''Performance da Empresa'', subtitle: ''Vendas, Despesas e Lucro: 2014-2017''}')
    .AddOption('bars', '''vertical''')
    .AddOption('hAxis', '{format: ''short''}')
  // .AddOption('colors', '[''#1b9e77'', ''#d95f02'', ''#7570b3'']')
    .Show;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TPGChart.New.Pie
    .AddValue('Trabalhano', 11)
    .AddValue('Comendo', 2)
    .AddValue('Assistindo TV', 3)
    .AddValue('Dormindo', 6)
    .AddValue('Estudando', 2)
    .AddOption('title', '''Atividades diárias''')
    .Show;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  TPGChart.New.Donut
    .AddValue('Grêmio', 52)
    .AddValue('Inter', 14)
    .AddValue('Chapecoense', 28)
    .AddValue('Flamengo', 6)
    .AddOption('title', '''Melhores Times''')
    .Show;
end;

end.
