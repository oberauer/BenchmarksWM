% The script makes us of the tight subplot code from
% http://www.mathworks.com/matlabcentral/fileexchange/27991-tight-subplot

% clear all; close all;
fname='junk';
omitflag=48;    %48 = ascii for 0
omitcode = -1; % omissions are coded as -1
plot = 0;     % 1 = plot, 2 = don't plot


tind = 1;
tlabel = 'abcdefghijklmnopqrstuvwxyz';

targlag = -3:3; % only look at lags -3:3

if plot
    figure(1)
    fig1 = tight_subplot(7,3,[.03 .01],[.1 .01],[.1 .01]);
    figure(2)
    fig2 = tight_subplot(7,3,[.03 .01],[.1 .01],[.1 .01]);
    figure(3)
    fig3 = tight_subplot(7,3,[.03 .01],[.1 .01],[.1 .01]);
    figure(4)
    fig4 = tight_subplot(7,3,[.03 .01],[.1 .01],[.1 .01]);
end

% these file names match the order of data in the panels, and the
% experiment names in Table 1

Fname = {'normed1201.txt','psim2dat.ed','FL04_1.dat',...
        'audtillycomb.txt','AudiVisDataa','AudiVisDatav',...
        'tarrynexp20','tarrynexp21',...
        'calendar1_24Vps.dat','calendar2_24Vp.dat','calendar3_40VP.dat','super4.prn',...
        'phonsimJML2.prn','serrhythmvis.dat','serrhythmaud.dat',...
        'DanCalendar2.dat','DanCalendar3.dat','DanCalendarX2.dat',...
        'pronall.txt','datarsp.txt','serialretrieval.txt'};

Ename = {'Farrell_Lewandowsky_2003_E1', 
         'Farrell_Lewandowsky_2003_E3', 
         'Farrell_Lewandowsky_2004_E1',
         'Nimmo_Lewandowsky_2006_E1',
         'Nimmo_Lewandowsky_2006_E2_aud',
         'Nimmo_Lewandowsky_2006_E2_vis',
         'Lewandowsky_Brown_Wright_Nimmo_2006_E1_quiet',
         'Lewandowsky_Brown_Wright_Nimmo_2006_E1_suppr',     
         'Lewandowsky_Geiger_Oberauer_2008_E1',
         'Lewandowsky_Geiger_Oberauer_2008_E2',
         'Lewandowsky_Geiger_Oberauer_2008_E3',
         'Lewandowsky_Geiger_Oberauer_2008_E4',
         'Lewandowsky_Farrell_2008_E2',
         'Farrell_2008_E1',
         'Farrell_2008_E2',
         'Lewandowsky_Geiger_Morrell_Oberauer_2010_E1',
         'Lewandowsky_Geiger_Morrell_Oberauer_2010_E2',
         'Lewandowsky_Geiger_Morrell_Oberauer_2010_E3',
         'Farrell_Lewandowsky_inpress_E1',
         'Farrell_Lewandowsky_inpress_E2',
         'Farrell_Lewandowsky_inpress_E3'};     
    
for F = 1:length(Fname)
    
    tname = Fname{F};
    ename = Ename{F};
    
    disp([char(tname), '; ', char(ename)]);
    
    switch char(tname)
        
        % the following are in an arbitrary (non chronological) order
        
        case 'tarrynexp20'
            %specify list items and responses for tarrynexp2
            fname = 'tarrynexp2.txt';
            fid = fopen(fname, 'r');
            format = '%d %d %d %d %1s %1s %1s %1s %1s %1s %1s %d %d %d %d %d %d %d %1s %1s %1s %1s %1s %1s %1s %d %d %d %d %d %d %d';
            ncols=32;
            listcols=5:11; % which columns contain the presented list?
            recallcols=19:25; % which columns contain the participants' output?
            intrusioncode=-9; % how are we coding extra-list intrusions?
            avflag = 0; % 0=quiet, 1=AS
            skipcoding = 0; % when skipcoding = 0, we need to do some extra work to convert raw data into a usable format
            
        case 'tarrynexp21'
            %specify list items and responses for tarrynexp2
            fname = 'tarrynexp2.txt';
            fid = fopen(fname, 'r');
            format = '%d %d %d %d %1s %1s %1s %1s %1s %1s %1s %d %d %d %d %d %d %d %1s %1s %1s %1s %1s %1s %1s %d %d %d %d %d %d %d';
            ncols=32;
            listcols=5:11;
            recallcols=19:25;
            intrusioncode=-9;
            avflag = 1; % 0=quiet, 1=AS
            skipcoding = 0;
            
        case 'serialretrieval.txt'
            %specify list items and responses for serialretrieval.txt
            fname = 'serialretrieval.txt';
            fid = fopen(fname, 'r');
            format = '%d %d %1s %1s %1s %1s %1s %1s %1s %f %f %f %f %f %f %1s %1s %1s %1s %1s %1s %1s %f %f %f %f %f %f %f';
            ncols=29;
            listcols=3:9;
            recallcols=16:22;
            intrusioncode=-9;
            omitflag = 46;
            skipcoding = 0;
            
        case 'datarsp.txt'
            %specify list items and responses for pure D control (datarsp.txt)
            fname = 'datarsp.txt';
            fid = fopen(fname, 'r');
            format = '%d %d %d %d %d %d %d %d %d %1s %1s %1s %1s %1s %1s %1s %1s %1s %1s %1s %1s';
            ncols=21;
            listcols=10:15;
            recallcols=16:21;
            intrusioncode=-9;
            skipcoding = 0;
            
        case 'audtillycomb.txt'
            %specify list items and responses for auditory matilda experiment 1
            fname = 'audtillycomb.txt';
            fid = fopen(fname, 'r');
            format = '%d %1s %1s %1s %1s %1s %1s %1s %f %f %f %f %f %f %1s %1s %1s %1s %1s %1s %1s %f %f %f %f %f %f %f';
            ncols=28;
            listcols=2:8;
            recallcols=15:21;
            intrusioncode=-9;
            skipcoding = 0;
            
        case 'AudiVisDataa'
            %specify list items and responses for auditory matilda experiment 2
            %Note: Auditory and visual must be analyzed separately
            fname = 'AudiVisData.txt';
            fid = fopen(fname, 'r');
            format = '%d %d %1s %1s %1s %1s %1s %1s %1s %1s %f %f %f %f %f %f %1s %1s %1s %1s %1s %1s %1s %f %f %f %f %f %f %f';
            ncols=30;  %if any other data ncol=30, need to set avflag to something that cant happen in col 2
            listcols=4:10;
            recallcols=17:23;
            avflag=97;   %97=a, 118=v
            intrusioncode=-9;
            skipcoding = 0;
            
        case 'AudiVisDatav'
            %specify list items and responses for auditory matilda experiment 2
            %Note: Auditory and visual must be analyzed separately
            fname = 'AudiVisData.txt';
            fid = fopen(fname, 'r');
            format = '%d %d %1s %1s %1s %1s %1s %1s %1s %1s %f %f %f %f %f %f %1s %1s %1s %1s %1s %1s %1s %f %f %f %f %f %f %f';
            ncols=30;  %if any other data ncol=30, need to set avflag to something that cant happen in col 2
            listcols=4:10;
            recallcols=17:23;
            avflag=118;   %97=a, 118=v
            intrusioncode=-9;
            skipcoding = 0;
            
        case 'DanCalendar2.dat'
            %specify list items and responses for dancalendar 2
            fname = 'DanCalendar2.dat';
            fid = fopen(fname, 'r');
            format = '%d %d %d %d %d %s %s %s %s %s %f %f %f %f %f %s %s %s %s %s %f %f %f %f %f';
            ncols=25;
            listcols=6:10;
            recallcols=16:20;
            intrusioncode=-9;
            avflag = 0;
            omitflag = 46;
            skipcoding = 0;
            
        case 'DanCalendar3.dat'
            %specify list items and responses for dancalendar 3 and x2
            fname = 'DanCalendar3.dat';
            fid = fopen(fname, 'r');
            format = '%d %d %d %d %s %s %s %s %s %f %f %f %f %f %s %s %s %s %s %f %f %f %f %f';
            ncols=24;
            listcols=5:9;
            recallcols=15:19;
            intrusioncode=-9;
            avflag = 0;
            omitflag = 46;
            skipcoding = 0;
            
        case 'DanCalendarX2.dat'
            %specify list items and responses for dancalendar 3 and x2
            fname = 'DanCalendarX2.dat';
            fid = fopen(fname, 'r');
            format = '%d %d %d %d %s %s %s %s %s %f %f %f %f %f %s %s %s %s %s %f %f %f %f %f';
            ncols=24;
            listcols=5:9;
            recallcols=15:19;
            intrusioncode=-9;
            avflag = 0;
            omitflag = 46;
            skipcoding = 0;
            
        case 'calendar1_24Vps.dat'
            fname = 'calendar2_24Vp.dat';
            fid = fopen(fname, 'r');
            format = '%d %d %d %d %d %s %s %s %s %s %s %s %s %s %s %f %f %f %f %f';
            ncols=20;
            listcols=6:10;
            recallcols=11:15;
            intrusioncode=-9;
            avflag = 0;
            skipcoding = 0;
            
        case 'calendar2_24Vp.dat'
            fname = 'calendar2_24Vp.dat';
            fid = fopen(fname, 'r');
            format = '%d %d %d %d %d %s %s %s %s %s %s %s %s %s %s %f %f %f %f %f';
            ncols=20;
            listcols=6:10;
            recallcols=11:15;
            intrusioncode=-9;
            avflag = 0;
            skipcoding = 0;
            
        case 'calendar3_40VP.dat'
            %specify list items and responses for calendar 3
            fname = 'calendar3_40VP.dat';
            fid = fopen(fname, 'r');
            format = '%d %d %d %d %d %d %s %s %s %s %s %s %s %s %s %s %f %f %f %f %f';
            ncols=21;
            listcols=7:11;
            recallcols=12:16;
            intrusioncode=-9;
            avflag = 0;
            skipcoding = 0;
            
        case 'phonsimJML2.prn'
            %specify list items and responses for phonsimjml2
            fname = 'phonsimjml2.prn';
            fid = fopen(fname, 'r');
            format = '%d %d %d %s %s %s %s %s %s %s %s %s %s %s %s';
            ncols=15;
            listcols=4:9;
            recallcols=10:15;
            intrusioncode=-9;
            avflag = 1;
            omitflag = 46;
            skipcoding = 0;
            
        case 'super4.prn'
            %specify list items and responses for super4
            fname = 'super4.prn';
            fid = fopen(fname, 'r');
            format = '%d %d %d %s %s %s %s %s %s %s %s %s %s';
            ncols=13;
            listcols=4:8;
            recallcols=9:13;
            intrusioncode=-9;
            avflag = 11;
            omitflag = 46;
            skipcoding = 0;
            
        case 'FL04_1.dat'
            fname = 'FL04_1.dat';
            load('FL04_1.dat');
            a = FL04_1;
            condn = 1;
            codedrecall = a(a(:,3)==condn,4:9);
            codedrecall(codedrecall>6) = -9;
            a = a(a(:,3)==condn,:);
            skipcoding = 1; % for the rest, skipcoding = 1 because the output is already coded up
            listcols = 1:6;
            
        case 'normed1201.txt'
            load normed1201.txt
            condn = 0; % 0 = dissimilar; >1 = similar
            a = normed1201;
            listcols = 1:6;
            codedrecall = a(a(:,3)==condn,10:15);
            codedrecall(codedrecall>6) = -9;
            a = a(a(:,3)==condn,:);
            skipcoding = 1;
            
        case 'pronall.txt'
            load pronall.txt
            a = pronall;
            codedrecall = a(:,4:10);
            listcols = 1:7;
            codedrecall(codedrecall>7)=-9;
            skipcoding=1;
            
        case 'psim2dat.ed'
            load psim2dat.ed;
            condn = 0; % 0 = dissimilar
            a = psim2dat;
            codedrecall = a(a(:,3)==condn,10:15);
            listcols = 1:6;
            a = a(a(:,3)==condn,:);
            skipcoding=1;
            
        case 'serrhythmaud.dat'
            load serrhythmaud.dat
            a = serrhythmaud;
            targCond = a(:,3)==0 & a(:,4)==0;
            codedrecall = a(targCond,11:16);
            listcols = 1:6;
            a = a(a(:,3)==targCond,:);
            skipcoding=1;
            
        case 'serrhythmvis.dat'
            load serrhythmvis.dat;
            a = serrhythmvis;
            targCond = a(:,3)==0 & a(:,4)==0;
            codedrecall = a(targCond,11:16);
            listcols = 1:6;
            a = a(a(:,3)==targCond,:);
            skipcoding=1;
            
        otherwise
            error('Unkown file');
    end
    
    ll=length(listcols);
    alag = (-(ll-1)):(ll-1); % range of possible lags
    tti=1;
    tarlagi = targlag*0;
    for tt = targlag
        targlagi(tti) = find(tt==alag); %#ok<SAGROW>
        tti=tti+1;
    end
    
    omitcode=-1;
    
    % Some of the data already codes recall in terms of output
    % position (e.g., 1 is the first presented item, 2 is the second
    % etc.). In cases where it isn't, we first need to recode the raw
    % responses
    
    if ~skipcoding
        %read data
        a = fscanf(fid, format, [ncols inf]);
        a=a';     %put trials into rows
        if strcmp(fname, 'AudiVisData.txt')
            avvec=a(:,3);
            a=a(avvec==avflag,:);
        elseif strcmp(fname, 'calendar1_24Vps.dat') || strcmp(fname, 'calendar2_24Vp.dat')
            avvec=a(:,4);
            a=a(avvec==avflag,:);
        elseif strcmp(fname, 'calendar3_40VP.dat')
            avvec=a(:,5);
            a=a(avvec==avflag,:);
        elseif strcmp(fname, 'DanCalendar2.dat')
            avvec=a(:,4);
            a=a(avvec==avflag,:);
        elseif strcmp(fname, 'DanCalendar3.dat') || strcmp(fname, 'DanCalendarX2.dat')
            avvec=a(:,3);
            a=a(avvec==avflag,:);
        elseif strcmp(fname, 'phonsimJML2.prn')
            avvec=a(:,3);
            a=a(avvec==avflag,:);
        elseif strcmp(fname, 'super4.prn')
            avvec=a(:,2);
            a=a(avvec==avflag,:);
        elseif strcmp(fname, 'tarrynexp2.txt')
            avvec=a(:,2);
            a=a(avvec==avflag,:);
        end
        
        fclose(fid);
        
        %now process matrix to recode recalls in terms of (input) serial
        %position
        codedrecall=zeros(size(a,1),ll);
        for i=1:size(a,1)
            for j=1:ll
                recitem = a(i,recallcols(j));
                if recitem ==omitflag
                    codedrecall(i,j)=omitcode;
                elseif recitem ==a(i,listcols(j))
                    codedrecall(i,j)=j;
                elseif  ~any(a(i,listcols)==recitem)
                    codedrecall(i,j)=intrusioncode;
                else
                    % this assumes no repetitions in input
                    codedrecall(i,j)= find(a(i,listcols)==recitem);
                end
            end
        end
        
    end  % if ~skipcoding
    
    %and do analysis
    [transG, transG1, lagCRP, lagCRP1, lagCRPfi, fillin, infill, fillin1, infill1, meanAcc,CRPalln, CRPall1n] = fillAnalysis(a(:,1), codedrecall);
    
    %write out for Benchmarks
    filename = [ename, '.dat']; 
    dlmwrite(filename, [a(:,1), codedrecall], 'delimiter', ' ');
    
%     fidout = fopen(filename, 'w');
%     formatstring = [repmat('%d ', 1, ll+1), ' %\n'];
%     fprintf(fidout, formatstring, [a(:,1), codedrecall] );
%     fclose(fidout);
    
    
    if plot
        figure(1)
        axes(fig1(tind))
        [a,b] = withinM(transG(:,targlagi));
        errorbar(targlag, a,b, '-k'); % careful--the a here replaces the one above!
        xlim([-3.5,3.5])
        ylim([0 1])
        text(-3, 0.8, tlabel(tind));
        
        figure(2)
        axes(fig2(tind))
        [a,b] = withinM(transG1(:,targlagi));
        errorbar(targlag, a,b, '-k');
        xlim([-3.5,3.5])
        ylim([0 1])
        text(-3, 0.8, tlabel(tind));
        
        figure(3)
        axes(fig3(tind))
        [a,b] = withinM(lagCRP(:,targlagi));
        errorbar(targlag, a,b, '-k');
        xlim([-3.5,3.5])
        ylim([0 1])
        text(-3, 0.8, tlabel(tind));
        if ttest(lagCRP(:,targlagi(5))-lagCRP(:,targlagi(3)));
            text(2,0.8, '*', 'FontSize', 14);
        end
        
        figure(4)
        axes(fig4(tind))
        [a,b] = withinM(lagCRP1(:,targlagi));
        errorbar(targlag, a,b, '-k');
        xlim([-3.5,3.5])
        ylim([0 1])
        text(-2, 0.8, tlabel(tind));
        if ttest(lagCRP1(:,targlagi(5))-lagCRP1(:,targlagi(3)));
            text(2,0.8, '*','FontSize', 14);
        end
        
        fillrat(tind) = sum(fillin)/sum(infill);
        fillrat1(tind) = sum(fillin1)/sum(infill1);
        
        tind = tind+1;
    end
    
end

% remInd = setdiff(1:21,[1 4 7 10 13 16 19]);
% figure(1)
% set(fig1(1:18),'XTickLabel',''); set(fig1(remInd),'YTickLabel','')
% suplabel('Displacement', 'x', [.1 .1 .85 .85]);
% suplabel('Proportion Responses', 'y', [.1 .1 .85 .85]);
% set(gcf,'Units','points');
% cz = get(gcf, 'Position');
% set(gcf, 'Position', [25 25 400 500])
% exportfig(gcf, 'displacement.eps');
%
% figure(2)
% set(fig1(1:18),'XTickLabel',''); set(fig2(remInd),'YTickLabel','')
% suplabel('Displacement', 'x', [.1 .1 .85 .85]);
% suplabel('Proportion Responses', 'y', [.1 .1 .85 .85]);
% set(gcf,'Units','points');
% cz = get(gcf, 'Position');
% set(gcf, 'Position', [25 25 400 500])
% exportfig(gcf, 'displacement1.eps');
%
% figure(3)
% set(fig1(1:18),'XTickLabel',''); set(fig3(remInd),'YTickLabel','')
% suplabel('Lag', 'x', [.1 .1 .85 .85]);
% suplabel('Proportion Responses', 'y', [.1 .1 .85 .85]);
% set(gcf,'Units','points');
% cz = get(gcf, 'Position');
% set(gcf, 'Position', [25 25 400 500])
% exportfig(gcf, 'lag-crp.eps');
%
% figure(4)
% set(fig1(1:18),'XTickLabel',''); set(fig4(remInd),'YTickLabel','')
% suplabel('Lag', 'x', [.1 .1 .85 .85]);
% suplabel('Proportion Responses', 'y', [.1 .1 .85 .85]);
% set(gcf,'Units','points');
% cz = get(gcf, 'Position');
% set(gcf, 'Position', [25 25 400 500])
% exportfig(gcf, 'lag-crp1.eps');