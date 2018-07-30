{$M 60000,0,655360}
program auswert;


const
     versuchsp=18;
     r=273;  {dots.pas}
     s=10;   {czeichen}
     pi=3.14159265363;
     On = 0;                          {const fr showpic}
     Off = 1;
     ScreenIsOn : BOOLEAN = TRUE;
     retention_interval = 10000;
     speed = 2.0; {movement.pas}
     block_number = 108;
     intertrialinterval = 1000;

type
     zweit_dat = array[1.. 25] of integer;


    trial_type = record haupt, zweit, expo: integer;
    {haupt=0: locations; =1: czeichen;
     zweit=0:warten; =1: movements; =2: colours}
                        stimulus, response: integer;
                        zweit_no: integer;
                        zweit_stim, zweit_response: zweit_dat;
     end;


var

    vpakt,ex,i,j,k:integer;
    refdots,refczeichen: integer;
    {Referenzdarbietungsdauer}
    vpdaten, eyedaten,tapdaten, ergebnis,eyeout: text;
    vp_nr: integer; vp,hilf: string;
    block: array[1..block_number] of trial_type;
    hasum,hasumn: array[1..3,0..1,0..2] of integer;
    zwsumn,zwsumk: array[1..3,0..1,1..2] of real;
    eyes,amp,vel,dur: array[0..1,0..2] of real;
    eyeno,kx: integer;
    trialtype: real;
    oamp,ovel,odur: real;

begin
  refczeichen:=337;
  refdots:=337;
  assign(ergebnis,'ergebnis'); rewrite(ergebnis);
  assign(eyeout,'eyeout');rewrite(eyeout);
  vpakt:=0;
  for vp_nr:=1 to versuchsp do if not (vp_nr in []) then
  begin
    for i:=0 to 1 do for j:=0 to 2 do
     begin eyes[i,j]:=0;
           vel[i,j]:=0;
           dur[i,j]:=0;
           amp[i,j]:=0;
     end;
    vpakt:=vpakt+1;
    str(vp_nr,vp);
    vp:='daten\raus.'+vp;
    assign(vpdaten,vp); reset(vpdaten);
    readln(vpdaten);
    str(vp_nr,vp);
    vp:='daten\tap.'+vp;
    assign(tapdaten,vp); reset(tapdaten);
    for i:=1  to block_number do with block[i] do
    begin
      if (i-1) mod 18 = 0 then
      begin
        if ((i-1) div 18 + 1 > 1) then close(eyedaten);
        str(vp_nr,vp);
        vp:='daten\tap'+vp+'vp';
        str((i-1) div 18 , hilf);
        vp:=vp+hilf+'.out';
        writeln(vp);
        assign(eyedaten,vp);
        reset(eyedaten)
      end;
      readln(vpdaten,haupt,expo,stimulus,response);
{      writeln(kx,trialtype,' ',eyeno);
}         if (haupt=0) then
          begin
            for j:=1 to 3 do if (expo = round(refdots + (j-3)*refdots/6))
            then ex:=j;
          end
          else
            for j:=1 to 3 do if (expo = round(refczeichen + (j-3)*refczeichen/6))
            then ex:=j;
      expo:=ex;
      read(vpdaten,zweit);
      if zweit>0 then
       begin if (zweit=2) then read(vpdaten,zweit_no)
                          else begin readln(tapdaten,zweit_stim[1],zweit_stim[2],zweit_stim[3]);readln(vpdaten);
                                     zweit_no:=1;{writeln('*',zweit_no)};
                               end;
           if (zweit=2) then for j:=1 to zweit_no do
             read(vpdaten,zweit_stim[j],zweit_response[j]);
       end;
      read(eyedaten,kx,trialtype,eyeno);
    if (haupt=0) and not(round(trialtype) in [10,30,50]) then writeln('Achtung');
    if (haupt=1) and not(round(trialtype) in [20,40,60]) then writeln('Achtung2');
    if (zweit=0) and not(round(trialtype) in [10,20]) then writeln('Achtung3');
    if (zweit=1) and not(round(trialtype) in [30,40]) then writeln('Achtung4');
    if (zweit=2) and not(round(trialtype) in [50,60]) then writeln('Achtung5');
    eyes[haupt,zweit]:=eyes[haupt,zweit]+eyeno/18;
     for j:=1 to eyeno do begin
      read(eyedaten,oamp,ovel,odur);
      amp[haupt,zweit]:=amp[haupt,zweit]+oamp/18;
      vel[haupt,zweit]:=vel[haupt,zweit]+ovel/18;
      dur[haupt,zweit]:=dur[haupt,zweit]+odur/18;
    end;

   end;
     close(vpdaten);
     close(eyedaten);
     close(tapdaten);
   for k:=1 to 3 do for i:=0 to 1 do for j:=0 to 2 do hasum[k,i,j]:=0;
   for k:=1 to 3 do for i:=0 to 1 do for j:=0 to 2 do hasumn[k,i,j]:=0;
   for k:=1 to 3 do for i:=0 to 1 do for j:=1 to 2 do zwsumk[k,i,j]:=0;
   for k:=1 to 3 do for i:=0 to 1 do for j:=1 to 2 do zwsumn[k,i,j]:=0;
   for i:=1 to block_number do with block[i] do
    begin
     hasumn[expo,haupt,zweit]:=hasumn[expo,haupt,zweit]+1;
     if (stimulus=response) then
       hasum[expo,haupt,zweit]:=hasum[expo,haupt,zweit] + 1;
    end;
   for k:=1 to 3 do for i:=0 to 1 do for j:=0 to 2 do
   begin
    writeln(hasumn[k,i,j]);
    hasum[k,i,j]:=round(hasum[k,i,j]/hasumn[k,i,j]*100);
     end;
   for i:=1 to block_number do with block[i] do
    begin if (zweit>0) then zwsumn[expo,haupt,zweit]:=zwsumn[expo,haupt,zweit]+zweit_no;
          for j:=1 to zweit_no do
          case zweit of
          0:;
          1: {if (zweit_stim[j]=zweit_response[j]) then}
             begin
              zwsumk[expo,haupt,zweit]:=zwsumk[expo,haupt,zweit]+
               {1/6 -} zweit_stim[3]/6*100;
              {writeln('*',zweit_no);}
             end;
          2: if ((zweit_stim[j]<8) and (zweit_response[j]=1))
                 or ((zweit_stim[j]>8) and (zweit_response[j]=2))
              then zwsumk[expo,haupt,zweit]:=zwsumk[expo,haupt,zweit] + 1/zweit_no/6;
          end;
    end;
   oamp:=0;
   ovel:=0;
   odur:=0;
  for i:=0 to 1 do for j:=0 to 2 do
  if eyes[i,j]>0 then
  begin
      oamp:=oamp + amp[i,j]/eyes[i,j]/6;
      ovel:=ovel + vel[i,j]/eyes[i,j]/6;
      odur:=odur + dur[i,j]/eyes[i,j]/6;
      amp[i,j]:=amp[i,j]/eyes[i,j];
      vel[i,j]:=vel[i,j]/eyes[i,j];
      dur[i,j]:=dur[i,j]/eyes[i,j];
  end;
  write(ergebnis,vp_nr:4);
  for k:=1 to 3 do for i:=0 to 1 do for j:=0 to 2 do write(ergebnis,hasum[k,i,j]:4);
  for k:=1 to 3 do for i:=0 to 1 do for j:=1 to 2 do write(ergebnis,round(zwsumk[k,i,j]*100):6);
  for k:=1 to 3 do for i:=0 to 1 do for j:=1 to 1 do writeln(round(zwsumk[k,i,j]));
  for i:=0 to 1 do for j:=0 to 2 do write(eyeout,round(eyes[i,j]*100):8);
{  for i:=0 to 1 do for j:=0 to 2 do write(eyeout,round(amp[i,j]*10):8);
  for i:=0 to 1 do for j:=0 to 2 do write(eyeout,round(vel[i,j]):8);
  for i:=0 to 1 do for j:=0 to 2 do write(eyeout,round(dur[i,j]):8);
} write(eyeout,round(oamp*10):8);
  write(eyeout,round(ovel):8);
  write(eyeout,round(odur):8);
  writeln(ergebnis);
  writeln(eyeout);
  end;
  close(ergebnis);
  close(eyeout);
end.
