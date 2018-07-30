% Read Murdock & Okadata file from Mike Kahana's web page, write out in a
% more accessible format

Continue = 1;

fid = fopen('mo1970.txt');
fid2 = fopen('MurdockOkada.1970.txt', 'a');

while Continue
    line1 = fgetl(fid);
    line2 = fgetl(fid);
    line3 = fgetl(fid);
    fprintf(fid2, '%s %s %s \n', line1, line2, line3);
    if line3 == -1, Continue = 0; end
end
fclose(fid);
fclose(fid2);
disp('done');


