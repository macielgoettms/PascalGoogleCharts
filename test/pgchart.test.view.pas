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
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses pgchart, pgchart.enumerations;

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

procedure TForm1.Button5Click(Sender: TObject);
begin
  TPGChart.New.Line
    .addColumn(tcNumber, 'Day')
    .addColumn(tcNumber, 'Guardians of the Galaxy')
    .addColumn(tcNumber, 'The Avengers')
    .addColumn(tcNumber, 'Transformers: Age of Extinction')
    .addRow([1, 37.8, 80.8, 41.8])
    .addRow([2, 30.9, 69.5, 32.4])
    .addRow([3, 25.4, 57, 25.7])
    .addRow([4, 11.7, 18.8, 10.5])
    .addRow([5, 11.9, 17.6, 10.4])
    .addRow([6, 8.8, 13.6, 7.7])
    .addRow([7, 7.6, 12.3, 9.6])
    .addRow([8, 12.3, 29.2, 10.6])
    .addRow([9, 16.9, 42.9, 14.8])
    .addRow([10, 12.8, 30.9, 11.6])
    .addRow([11, 5.3, 7.9, 4.7])
    .addRow([12, 6.6, 8.4, 5.2])
    .addRow([13, 4.8, 6.3, 3.6])
    .addRow([14, 4.2, 6.2, 3.4])
    .AddOption('chart',
    '{title: ''Box Office Earnings in First Two Weeks of Opening'',' + sLineBreak +
    ' subtitle: ''in millions of dollars (USD)''}')
    .AddOption('width', '900')
    .AddOption('height', '500')
    .AddOption('axes','{x: {0: {side: ''top''}}}')
    .Show;
end;

end.
