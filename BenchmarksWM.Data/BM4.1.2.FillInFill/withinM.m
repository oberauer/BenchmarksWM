function [means, ses] = withinM(smat)

means = nanmean(smat,1);

smeans = nanmean(smat,2);

ses = nanstd(bsxfun(@minus,smat,smeans))./sqrt(sum(~isnan(smat)));
ses = ses .* tinv(.975,sum(~isnan(smat))-1);