%% This part of the code reproduces Figure 1 of the paper %%

% Housekeeping
clear; clc; close all; 

temp = matlab.desktop.editor.getActive;

cd(fileparts(temp.Filename));

% Read the data
yields_data = readtable("yields_data.xls"); 
date = table2array(yields_data(:,1)); 
spain = table2array(yields_data(:,2)); 
greece = table2array(yields_data(:,3)); 
portugal = table2array(yields_data(:,4)); 
italy = table2array(yields_data(:,5)); 
ireland = table2array(yields_data(:,6)); 
germany = table2array(yields_data(:,7)); 

% Note that Matlab reads Greek yield in 2015-07-10 as 0
% Need to correct this

greece_index = find(date == datetime('2015-07-01')); 

greece(greece_index) = nan; 


% Gather data for PIIGS and construct yields w.r.t. German Yield

piigs_yields = [spain portugal greece italy ireland]; 
piigs_yields = piigs_yields - germany; 


figure
hold on
plot(date,piigs_yields, 'LineWidth', 1.5)
legend('Spain', 'Portugal', 'Greece', 'Italy', 'Ireland', 'Interpreter', 'Latex', 'Location', 'best')
ylabel("Spread against German yield (in \%)", 'Interpreter','latex')
set(gcf, 'color','white')
set(gca,'TickLabelInterpreter','latex')
axis tight
grid on
grid minor

%% This part of the code reproduces Figure 2 of the paper %%

% Housekeeping

clear; clc; close all;

s_t_data = readtable('s_t.xlsx');


date = table2array(s_t_data(:,1)); 
synch = table2array(s_t_data(:,2));

figure
plot(date,synch, 'LineWidth',1.5)
set(gcf, 'color', 'white')
ylabel({'Cross-country Standard deviation of'; 'long-term government bond yields'}, 'Interpreter', 'Latex')
%pbaspect([1 .5 .5])


greece = datetime('2001-01-01'); 
slovenia =  datetime('2007-01-01'); 
slovakia =  datetime('2009-01-01');
latvia =  datetime('2014-01-01');
lithuania =  datetime('2015-01-01');


xline([greece slovenia slovakia latvia lithuania], '--k', ...
    {'Greece', 'Slovenia', 'Slovakia', 'Latvia', 'Lithuania'}, 'LineWidth', 1.5,...
    'Interpreter', 'Latex', 'Fontsize', 12)
set(gca,'TickLabelInterpreter','latex')
axis tight
grid on
grid minor

