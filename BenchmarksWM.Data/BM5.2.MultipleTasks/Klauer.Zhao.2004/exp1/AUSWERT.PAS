{$M 60000,0,655360}
program auswert;


const
     versuchsp=20;
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
    vpdaten, zweitno,ergebnis: text;
    vp_nr: integer; vp: string;
    block: array[1..block_number] of trial_type;
    hasum,hasumn: array[1..3,0..1,0..2] of integer;
    zwsumn,zwsumk: array[1..3,0..1,1..2] of real;
    zwno: array[0..1,1..2] of real;

begin
  refczeichen:=337;
  refdots:=337;
  assign(ergebnis,'ergebnis'); rewrite(ergebnis);
  assign(zweitno,'zweitno'); rewrite(zweitno);
  vpakt:=0;
  for vp_nr:=1 to versuchsp do if not (vp_nr in []) then
  begin
    vpakt:=vpakt+1;
    str(vp_nr,vp);
    vp:='daten\raus.'+vp;
    assign(vpdaten,vp); reset(vpdaten);
    readln(vpdaten);
    for i:=1  to block_number do with block[i] do
    begin
      readln(vpdaten,haupt,expo,stimulus,response);
         if (haupt=0) then
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
       begin read(vpdaten,zweit_no);
           for j:=1 to zweit_no do
             read(vpdaten,zweit_stim[j],zweit_response[j]);
       end;
    end;
    close(vpdaten);
   for k:=1 to 3 do for i:=0 to 1 do for j:=0 to 2 do hasum[k,i,j]:=0;
   for k:=1 to 3 do for i:=0 to 1 do for j:=0 to 2 do hasumn[k,i,j]:=0;
   for k:=1 to 3 do for i:=0 to 1 do for j:=1 to 2 do zwsumk[k,i,j]:=0;
   for k:=1 to 3 do for i:=0 to 1 do for j:=1 to 2 do zwsumn[k,i,j]:=0;
   for i:=0 to 1 do for j:=1 to 2 do zwno[i,j]:=0;
   for i:=1 to block_number do with block[i] do
    begin
     hasumn[expo,haupt,zweit]:=hasumn[expo,haupt,zweit]+1;
     if (stimulus=response) then
       hasum[expo,haupt,zweit]:=hasum[expo,haupt,zweit] + 1;
    end;
   for k:=1 to 3 do for i:=0 to 1 do for j:=0 to 2 do
    hasum[k,i,j]:=round(hasum[k,i,j]/hasumn[k,i,j]*100);
   for i:=1 to block_number do with block[i] do
    begin
          if zweit>0 then zwno[haupt,zweit]:=zwno[haupt,zweit]+zweit_no/18;
          if (zweit>0) then zwsumn[expo,haupt,zweit]:=zwsumn[expo,haupt,zweit]+zweit_no;
          for j:=1 to zweit_no do
          case zweit of
          0:;
          1: if (zweit_stim[j]=zweit_response[j]) then
             zwsumk[expo,haupt,zweit]:=zwsumk[expo,haupt,zweit] + 1/zweit_no/6;
          2: if ((zweit_stim[j]<8) and (zweit_response[j]=1))
                 or ((zweit_stim[j]>8) and (zweit_response[j]=2))
              then zwsumk[expo,haupt,zweit]:=zwsumk[expo,haupt,zweit] + 1/zweit_no/6;
          end;
    end;
  write(ergebnis,vp_nr:4);
  for k:=1 to 3 do for i:=0 to 1 do for j:=0 to 2 do write(ergebnis,hasum[k,i,j]:4);
  for k:=1 to 3 do for i:=0 to 1 do for j:=1 to 2 do write(ergebnis,round(zwsumk[k,i,j]*100):6);
  writeln(ergebnis);
  for i:=0 to 1 do for j:=1 to 2 do write(zweitno,round(100*zwno[i,j]):6);
  writeln(zweitno);
  end;
  close(ergebnis);
  close(zweitno);
end.
